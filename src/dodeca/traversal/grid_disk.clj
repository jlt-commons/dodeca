;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.traversal.grid-disk
  (:require [dodeca.core.face-adjacency :refer [walk-faces]]
            [dodeca.core.origin :refer [origins]]
            [dodeca.core.serialization :as s :refer [first-curve-resolution]]
            [dodeca.coverings.slot-runs :refer [compact-cells to-covering]]
            [dodeca.traversal.triple-cells :refer [cell-ids->triples triple-neighbors triple-cell-key triple-cell->id]]))

;; One BFS ring: its dedup :keys, and its :cells as [origin-id quintant x y z]
(def ^:private empty-ring {:keys #{} :cells []})

(defn- add-cell
  "`next` with the cell added, unless it is already in one of the three live rings."
  [next prev current [origin-id quintant x y z :as cell]]
  (let [k (triple-cell-key origin-id quintant x y z)]
    (if (or (contains? (:keys prev) k) (contains? (:keys current) k) (contains? (:keys next) k))
      next
      (-> next (update :keys conj k) (update :cells conj cell)))))

(defn- push-cell-ids
  "Encode a ring's cells as cell IDs, appending them to `out`."
  [out cells curve-level resolution]
  (into out (map (fn [[o q x y z]] (triple-cell->id o q x y z curve-level resolution))) cells))

(defn- grid-disk*
  "BFS grid disk in triple space, with progressive compaction.

  Neighbors come from the per-flavor triple deltas, plus the boundary delta
  tables for cells on a quintant edge, so no cell is decoded and each is
  encoded exactly once, when it leaves the window.

  Uses a sliding window for deduplication: only the previous and current
  frontier rings are kept (BFS guarantees cells two or more rings behind the
  frontier can never be re-discovered). Evicted interior cells are
  periodically compacted to reduce memory pressure."
  [cell-id k edge-only]
  (let [{:keys [origin resolution]} (s/deserialize cell-id)]
    (cond
      (zero? k) (to-covering [cell-id] resolution)

      ;; The cells are the 12 dodecahedron faces
      (zero? resolution)
      (to-covering (map (fn [face] (s/serialize {:origin (nth origins face) :segment 0 :s 0 :resolution 0}))
                        (walk-faces [(:id origin)] (constantly true) k))
                   0)

      :else
      (let [curve-level (+ (- resolution first-curve-resolution) 1)
            max-row (dec (bit-shift-left 1 curve-level))
            [seed] (cell-ids->triples [cell-id])]
        ;; The seed is cell-id already, so it goes straight to the output
        (loop [ring 1
               interior [cell-id]
               prev-frontier empty-ring
               frontier (add-cell empty-ring empty-ring empty-ring seed)]
          (if (> ring k)
            ;; Merge remaining boundary rings with compacted interior
            (to-covering (-> interior
                             (push-cell-ids (:cells prev-frontier) curve-level resolution)
                             (push-cell-ids (:cells frontier) curve-level resolution))
                         resolution)
            (let [next-frontier (reduce (fn [next [o q x y z]]
                                          (reduce #(add-cell %1 prev-frontier frontier %2)
                                                  next
                                                  (triple-neighbors o q x y z max-row edge-only)))
                                        empty-ring
                                        (:cells frontier))
                  ;; The seed ring is expanded; drop its cell so it isn't encoded again (its key stays)
                  frontier (if (= ring 1) (assoc frontier :cells []) frontier)
                  ;; Evict prev-frontier: these cells are two rings behind the
                  ;; new frontier and can never be re-discovered by BFS
                  interior (push-cell-ids interior (:cells prev-frontier) curve-level resolution)
                  ;; Progressively compact interior to reduce memory pressure
                  interior (if (> (count interior) 100) (compact-cells interior) interior)]
              (recur (inc ring) interior frontier next-frontier))))))))

(defn grid-disk
  "The grid disk of edge-sharing neighbors within k hops: the compacted cell
  IDs, center included, sorted in curve order, then a compaction marker
  recording the resolution. `dodeca.coverings.compact/uncompact` gives all the
  cells at the input resolution."
  [cell-id k]
  (grid-disk* cell-id k true))

(defn grid-disk-vertex
  "The grid disk of all neighbors (edge and vertex sharing) within k hops, as
  `grid-disk` gives it."
  [cell-id k]
  (grid-disk* cell-id k false))
