;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.coverings.slot-runs
  "The covering engine. Each cell covers a block of leaf slots (see
  dodeca.core.serialization), so a set of cells is a list of sorted, disjoint
  slot runs: cells are turned into slot runs, set operations merge runs, and
  runs are turned back into the coarsest cells covering them. Nothing is
  uncompacted.

  Slot runs are A5's flattened [lo0 hi0 lo1 hi1 ...], as a vector. A set of
  cells (A5's Cells) is any sequence of IDs. Slots reach WORLD_SLOTS, past
  2^63, so they compare unsigned and add without overflow checks."
  (:require [dodeca.core.compaction-marker :refer [compaction-marker compaction-marker?]]
            [dodeca.core.serialization :as s]
            [dodeca.utils.unsigned :as u :refer [u< u<=]]))

(defn append-slot-run
  "Append a slot run [lo, hi) to sorted runs starting at or before lo, merging
  if they touch or overlap."
  [runs lo hi]
  (let [last (dec (count runs))]
    (if (and (pos? last) (u<= lo (nth runs last)))
      (if (u< (nth runs last) hi) (assoc runs last hi) runs)
      (conj runs lo hi))))

(defn- merge-cells
  "Merge cells, in the given order, into sorted and disjoint slot runs. With
  `check-order`, stop and return nil at the first cell starting before the
  one before it."
  [cells check-order]
  (loop [cells (seq cells)
         previous-lo 0
         out []]
    (if-let [[cell & more] cells]
      (if (compaction-marker? cell)
        (recur more previous-lo out)
        (let [lo (s/cell-first-slot cell)
              hi (unchecked-add lo (s/cell-slot-count cell))]
          (if (and check-order (u< lo previous-lo))
            nil
            ;; Drop earlier runs this one contains, then merge with the one before it
            (let [out (loop [out out]
                        (let [top (- (count out) 2)]
                          (if (and (>= top 0) (u<= lo (nth out top)) (u<= (nth out (inc top)) hi))
                            (recur (subvec out 0 top))
                            out)))]
              (recur more lo (append-slot-run out lo hi))))))
      out)))

(defn- id-inside-run?
  "Whether a cell's ID lies inside its own slot run, so IDs sort like runs: res 1-29."
  [cell]
  (let [tag (u/lowest-set-bit cell)]
    (and (not= cell s/world-cell)
         (not= tag (nth s/resolution-tags 0))
         (zero? (bit-and tag s/res30-tag-bits)))))

(defn- sort-cells
  "Cells sorted by a slot inside each cell's run: then disjoint runs come out
  in order, and a run can only be preceded by runs it contains or that contain
  it. Below res 30 and above res 0 the ID itself is such a slot, so a sort of
  the IDs does it."
  [cells]
  (if (every? id-inside-run? cells)
    (sort u/unsigned-compare cells)
    (sort-by (fn [cell] (if (compaction-marker? cell) s/world-slots (s/cell-first-slot cell)))
             u/unsigned-compare
             cells)))

(defn to-slot-runs
  "The slot runs covered by a set of cells, sorted and merged. Compaction
  markers are skipped.

  Coverings come sorted in curve order, so the cells are first merged as
  given, checking the order as they go; only input found out of order is
  sorted, and merged again."
  [cells]
  (or (merge-cells cells true)
      (merge-cells (sort-cells cells) false)))

(defn slot-runs->cells
  "The coarsest cells covering the slot runs, in curve order. Runs built from
  cells at resolution r or coarser are aligned to res-r cells, so no cell
  finer than r is needed."
  [runs]
  (persistent!
   (reduce
    (fn [out i]
      (let [hi (nth runs (inc i))]
        (loop [lo (nth runs i)
               out out]
          (cond
            (not (u< lo hi)) out
            (and (zero? lo) (= hi s/world-slots)) (conj! out s/world-cell)
            :else
            (let [whole (when (zero? (bit-and lo s/s-mask))
                          ;; Whole origins, then whole quintants
                          (cond
                            (and (zero? (mod (u/ushr lo s/quintant-shift) 5))
                                 (u<= (unchecked-add lo s/origin-slots) hi))
                            [(s/slot->cell lo 0) s/origin-slots]
                            (u<= (unchecked-add lo s/quintant-slots) hi)
                            [(s/slot->cell lo 1) s/quintant-slots]))]
              (if whole
                (recur (unchecked-add lo (second whole)) (conj! out (first whole)))
                ;; The coarsest curve-level cell that starts at lo and fits: its
                ;; span is 4^k slots, at most the alignment of lo and the length
                ;; of the run. A5 takes floor(log2) of the length as a double,
                ;; which can round up, and shrinks the cell until it fits; the
                ;; exact bit length gives the same cell.
                (let [offset (bit-and lo s/s-mask)
                      bits (if (zero? offset) 56 (u/trailing-zeros offset))
                      fit (dec (u/bit-length (unchecked-subtract hi lo)))
                      bits (min bits fit)
                      r (loop [r (max 2 (long (Math/ceil (/ (- 60 bits) 2.0))))]
                          (if (u< hi (unchecked-add lo (nth s/slot-counts r))) (recur (inc r)) r))]
                  (recur (unchecked-add lo (nth s/slot-counts r)) (conj! out (s/slot->cell lo r))))))))))
    (transient [])
    (range 0 (count runs) 2))))

(defn slot-runs->covering
  "The coarsest cells covering slot runs, in curve order, then the compaction
  marker for `resolution`."
  [runs resolution]
  (cond-> (slot-runs->cells runs)
    (>= resolution 0) (conj (compaction-marker resolution))))

(defn compact-cells
  "Compact cells without appending a compaction marker: the coarsest cells
  covering them, sorted in curve order. For internal use on intermediate
  results."
  [cells]
  (slot-runs->cells (to-slot-runs cells)))

(defn to-covering
  "Compact cells, at resolution `resolution` or coarser, into a covering: the
  coarsest cells covering them, sorted in curve order, then the compaction
  marker for `resolution`. The resolution is given, so an empty fill still
  records it."
  [cells resolution]
  (slot-runs->covering (to-slot-runs cells) resolution))
