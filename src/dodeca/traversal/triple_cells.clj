;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.traversal.triple-cells
  "Cells handled in triple space, [origin-id quintant x y z], by the traversal
  algorithms that walk many neighboring cells: they key and dedup cells as
  plain integers and encode a cell to its ID only when it is output.

  A5 visits neighbors through callbacks; the functions here return them, in
  the order A5 visits them."
  (:require [dodeca.core.origin :refer [origins quintant->segment segment->quintant]]
            [dodeca.core.serialization :as s :refer [first-curve-resolution quintant-shift]]
            [dodeca.core.tiling :refer [get-pentagon-center]]
            [dodeca.lattice.index :refer [s->triple triple-flavor triple-in-bounds? triple->s]]
            [dodeca.projections.dodecahedron :as dodecahedron]
            [dodeca.traversal.lattice-boundary :refer [get-boundary-neighbor-triples]]
            [dodeca.traversal.neighbors :refer [neighbor-deltas]]))

;; A cell's key packs the quintant (origin.id * 5 + quintant, < 60), parity,
;; and the low KEY_BITS bits of -x and -z (y follows) into 51 bits. Up to
;; curve level 21 the coordinates fit whole; above it, two cells of one
;; quintant share a key only if they are 2^22 rows apart, and a walk holding
;; both would need ~2^21 steps (~10^12 cells for a disk), far past what fits
;; in memory.
(def ^:private key-bits 22)
(def ^:private key-mask (dec (bit-shift-left 1 key-bits)))
(def ^:private key-side (bit-shift-left 1 key-bits))

(defn triple-cell-key
  "The integer key of a cell, unique among the cells of any one traversal."
  [origin-id quintant x y z]
  (+ (+ (+ (+ (* (+ (* (bit-and (- 0 x) key-mask) key-side) (bit-and (- 0 z) key-mask)) 2) x) y) z)
     (* (* (* (+ (* origin-id 5) quintant) 2) key-side) key-side)))

;; Each of the 60 quintants, by triple quintant (origin.id * 5 + quintant): its
;; :segment, curve :orientation and slot :prefix (its place in ID order,
;; shifted into the top bits of a slot); and :triple-quintant-by-id-order, the
;; triple quintant in each place of ID order.
(def ^:private quintant-tables
  (delay
    (let [entries (for [origin origins
                        quintant (range 5)]
                    (let [{:keys [segment orientation]} (quintant->segment quintant origin)]
                      {:segment segment
                       :orientation orientation
                       :id-order (+ (* 5 (:id origin)) (mod (+ (- segment (:first-quintant origin)) 5) 5))
                       :triple-quintant (+ (* 5 (:id origin)) quintant)}))]
      {:segment (mapv :segment entries)
       :orientation (mapv :orientation entries)
       :prefix (mapv #(bit-shift-left (:id-order %) quintant-shift) entries)
       :triple-quintant-by-id-order (reduce (fn [v e] (assoc v (:id-order e) (:triple-quintant e)))
                                            (vec (repeat 60 nil))
                                            entries)})))

(defn quintant-segment [] (:segment @quintant-tables))
(defn quintant-orientation [] (:orientation @quintant-tables))
(defn quintant-prefix [] (:prefix @quintant-tables))
(defn triple-quintant-by-id-order [] (:triple-quintant-by-id-order @quintant-tables))

(defn triple-cell->id
  "The cell ID of a cell given in triple space."
  [origin-id quintant x y z curve-level resolution]
  (let [{:keys [segment orientation]} @quintant-tables
        q (+ (* origin-id 5) quintant)]
    (s/serialize {:origin (nth origins origin-id)
                  :segment (nth segment q)
                  :s (triple->s {:x x :y y :z z} curve-level (nth orientation q))
                  :resolution resolution})))

(defn cell-ids->triples
  "Decode cell IDs (each at resolution 1 or above) into triple space,
  appended to `out` as [origin-id quintant x y z]."
  ([cell-ids] (cell-ids->triples cell-ids []))
  ([cell-ids out]
   (reduce (fn [out cell-id]
             (let [{:keys [origin segment s resolution]} (s/deserialize cell-id)
                   {:keys [quintant orientation]} (segment->quintant segment origin)
                   {:keys [x y z]} (s->triple s (+ (- resolution first-curve-resolution) 1) orientation)]
               (conj out [(:id origin) quintant x y z])))
           out
           cell-ids)))

(defn triple-cell-center
  "The center of a cell given in triple space, on the sphere."
  [origin-id quintant x y z curve-level max-row]
  (let [triple {:x x :y y :z z}]
    (dodecahedron/inverse (get-pentagon-center curve-level quintant triple (triple-flavor triple max-row))
                          origin-id)))

(defn- boundary-neighbors
  "The neighbors of a cell on a quintant edge that lie across it."
  [origin-id quintant triple max-row edge-only skip-corners]
  (get-boundary-neighbor-triples {:triple triple
                                  :parity (+ (:x triple) (:y triple) (:z triple))
                                  :source-quintant quintant
                                  :origin (nth origins origin-id)
                                  :max-row max-row}
                                 edge-only skip-corners []))

(defn triple-neighbors
  "Every neighbor of a cell given in triple space, as A5's
  forEachTripleNeighbor visits them: within its quintant the fixed per-flavor
  triple deltas, and, for a cell on a quintant edge (x = 0, z = 0 or y =
  maxRow), the boundary delta tables. `edge-only` restricts to the 5
  edge-sharing neighbors; otherwise the vertex-only neighbors come too. A
  neighbor may appear more than once."
  [origin-id quintant x y z max-row edge-only]
  (let [triple {:x x :y y :z z}
        ;; Within the quintant: the fixed per-flavor deltas
        deltas (get-in neighbor-deltas [(triple-flavor triple max-row) (if edge-only :edge :all)])
        within (reduce (fn [out d]
                         (let [neighbor {:x (+ x (:x d)) :y (+ y (:y d)) :z (+ z (:z d))}]
                           (if (triple-in-bounds? neighbor max-row)
                             (conj out [origin-id quintant (:x neighbor) (:y neighbor) (:z neighbor)])
                             out)))
                       []
                       deltas)]
    ;; Across a quintant edge: the boundary delta tables
    (if (or (zero? x) (zero? z) (= y max-row))
      (into within (boundary-neighbors origin-id quintant triple max-row edge-only false))
      within)))

(defn for-each-triple-neighbor
  "Call (visit origin-id quintant x y z) for every neighbor `triple-neighbors` gives."
  [origin-id quintant x y z max-row edge-only visit]
  (doseq [[o q nx ny nz] (triple-neighbors origin-id quintant x y z max-row edge-only)]
    (visit o q nx ny nz)))

(defn walk-triple-cells
  "Breadth-first walk from `seeds` (triples) through neighbors (edge and
  vertex, across quintant edges too): each cell reached is passed to
  (expand origin-id quintant x y z) once, and the walk continues from those it
  returns true for. The seeds count as reached but are not passed to `expand`."
  [seeds max-row expand]
  (loop [visited (into #{} (map #(apply triple-cell-key %)) seeds)
         frontier seeds]
    (when (seq frontier)
      (let [[visited next]
            (reduce (fn [[visited next] [o q x y z]]
                      (reduce (fn [[visited next :as acc] [no nq nx ny nz :as cell]]
                                (let [k (triple-cell-key no nq nx ny nz)]
                                  (if (contains? visited k)
                                    acc
                                    [(conj visited k) (if (expand no nq nx ny nz) (conj next cell) next)])))
                              [visited next]
                              (triple-neighbors o q x y z max-row false)))
                    [visited []]
                    frontier)]
        (recur visited next)))))

(defn lattice-neighbors
  "Every lattice neighbor of a cell given in triple space, as A5's
  forEachLatticeNeighbor visits them: the 3 parity-valid single-axis moves
  within its quintant, and, for a cell on a quintant edge, its edge-sharing
  boundary neighbors, but not the vertex corner, which the lattice moves
  don't traverse either. A neighbor may appear more than once."
  [origin-id quintant x y z max-row]
  ;; Within the quintant: +1 on one axis from a parity 0 triple, -1 from parity 1
  (let [step (if (zero? (+ x y z)) 1 -1)
        within (cond-> []
                 (triple-in-bounds? {:x (+ x step) :y y :z z} max-row) (conj [origin-id quintant (+ x step) y z])
                 (triple-in-bounds? {:x x :y (+ y step) :z z} max-row) (conj [origin-id quintant x (+ y step) z])
                 (triple-in-bounds? {:x x :y y :z (+ z step)} max-row) (conj [origin-id quintant x y (+ z step)]))]
    ;; Across a quintant edge: the boundary delta tables
    (if (or (zero? x) (zero? z) (= y max-row))
      (into within (boundary-neighbors origin-id quintant {:x x :y y :z z} max-row true true))
      within)))
