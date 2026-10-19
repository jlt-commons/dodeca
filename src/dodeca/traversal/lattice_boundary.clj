;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.traversal.lattice-boundary
  "The neighbors of a cell that lie across its quintant's edges. A cell given
  in triple space is [origin-id quintant x y z]; A5 packs these into flat
  number arrays, a vector of such tuples here."
  (:require [dodeca.core.face-adjacency :refer [face-adjacency seam-triple]]
            [dodeca.lattice.index :refer [triple-flavor triple-in-bounds?]]
            [dodeca.traversal.neighbors :refer [neighbor-deltas]]))

;; A neighbor delta: [dx dy dz edge-sharing?]

(def left-edge-deltas
  "Cross-quintant left-edge deltas (source z=0), indexed by
  parity * 2 + (y odd ? 1 : 0). Applied to the swapped base triple [0, y, x]
  in the previous quintant."
  [;; parity=0, y even
   [[0 0 0 true] [0 0 1 false]]
   ;; parity=0, y odd
   [[0 0 0 true] [0 1 0 true] [0 -1 1 false] [0 1 -1 false]]
   ;; parity=1, y even
   []
   ;; parity=1, y odd
   [[0 -1 0 true] [0 0 -1 false]]])

(def right-edge-deltas
  "Cross-quintant right-edge deltas (source x=0), indexed by
  parity * 2 + (y odd ? 1 : 0). Applied to the swapped base triple [z, y, 0]
  in the next quintant."
  [;; parity=0, y even
   [[0 0 0 true] [0 1 0 true] [-1 1 0 false] [1 -1 0 false]]
   ;; parity=0, y odd
   [[0 0 0 true] [1 0 0 false]]
   ;; parity=1, y even
   [[0 -1 0 true] [-1 0 0 false]]
   ;; parity=1, y odd
   []])

(defn- push-triple
  "out with the cell appended, if the triple is a valid cell."
  [out x y z origin-id quintant max-row]
  (if (triple-in-bounds? {:x x :y y :z z} max-row)
    (conj out [origin-id quintant x y z])
    out))

(defn- push-deltas
  "Apply a delta table to a base triple, appending each valid cell."
  [out base deltas edge-only origin-id quintant max-row]
  (reduce (fn [out [dx dy dz edge]]
            (if (and edge-only (not edge))
              out
              (push-triple out (+ (:x base) dx) (+ (:y base) dy) (+ (:z base) dz) origin-id quintant max-row)))
          out
          deltas))

(defn get-boundary-neighbor-triples
  "Every neighbor that lies outside the source cell's quintant, appended to
  `out` as [origin-id quintant x y z]: cross-quintant lateral edges,
  cross-face base edge, apex (face center), and (unless `skip-corners`) the
  [-maxRow, maxRow, 0] vertex corner. The within-quintant ±1 candidates are
  NOT covered here: callers generate those directly.

  Only cells on a quintant edge (x = 0, z = 0 or y = maxRow) have any. The
  result may contain duplicates; callers deduplicate.

  ctx is {:triple :parity :source-quintant :origin :max-row}; `edge-only`
  drops apex non-adjacent quintants and other vertex-only neighbors;
  `skip-corners` drops the [-maxRow, maxRow, 0] corner, for callers whose
  connectivity (e.g. lattice ±1 moves) doesn't traverse that vertex."
  [{:keys [triple parity source-quintant origin max-row]} edge-only skip-corners out]
  (let [{:keys [x y z]} triple
        origin-id (:id origin)
        delta-index (+ (* parity 2) (if (odd? y) 1 0))]
    (cond-> out
      ;; Left edge (z=0): neighbor in previous quintant at swapped [0, y, x]
      (zero? z)
      (push-deltas {:x 0 :y y :z x} (nth left-edge-deltas delta-index) edge-only
                   origin-id (mod (+ (- source-quintant 1) 5) 5) max-row)

      ;; Right edge (x=0): neighbor in next quintant at swapped [z, y, 0]
      (zero? x)
      (push-deltas {:x z :y y :z 0} (nth right-edge-deltas delta-index) edge-only
                   origin-id (mod (inc source-quintant) 5) max-row)

      ;; Base edge (y=maxRow): across the face seam the lattice continues, so
      ;; the neighbors on the adjacent face are those of the cell's image there
      (= y max-row)
      (as-> out
        (let [[adj-face-id adj-quintant] (get-in face-adjacency [origin-id source-quintant])
              image (seam-triple triple max-row)
              ;; The image's pentagon is the cell's turned half-way round: its
              ;; flavor's parity bit flipped
              deltas (nth neighbor-deltas (bit-xor (triple-flavor triple max-row) 1))]
          (reduce (fn [out d]
                    ;; Only steps back towards the seam (dy < 0) can land inside the neighbor quintant
                    (if (neg? (:y d))
                      (push-triple out (+ (:x image) (:x d)) (+ (:y image) (:y d)) (+ (:z image) (:z d))
                                   adj-face-id adj-quintant max-row)
                      out))
                  out
                  (if edge-only (:edge deltas) (:all deltas)))))

      ;; Apex [0,0,0]: cells from all 5 quintants meet at the face center
      (and (zero? x) (zero? y) (zero? z))
      (as-> out
        (reduce (fn [out q]
                  (let [distance (min (mod (+ (- q source-quintant) 5) 5) (mod (+ (- source-quintant q) 5) 5))]
                    (if (or (= q source-quintant) (and edge-only (not= distance 1)))
                      out
                      (push-triple out 0 0 0 origin-id q max-row))))
                out
                (range 5)))

      ;; Base-left corner [-maxRow, maxRow, 0]: 3 dodecahedron faces meet at
      ;; this vertex. The symmetric base-right corner is implicitly covered:
      ;; its cross-quintant and cross-face paths land on the [-maxRow, maxRow,
      ;; 0] cell of neighboring quintants.
      (and (not skip-corners) (= x (- max-row)) (= y max-row) (zero? z))
      (as-> out
        (let [prev-quintant (mod (+ (- source-quintant 1) 5) 5)
              ;; Vertex neighbor 1: across the previous quintant's base edge
              [prev-adj-face-id prev-adj-quintant] (get-in face-adjacency [origin-id prev-quintant])
              ;; Vertex neighbor 2: adjacent quintant on the primary cross-face
              [cross-face-id cross-quintant] (get-in face-adjacency [origin-id source-quintant])]
          (-> out
              (push-triple x y z prev-adj-face-id prev-adj-quintant max-row)
              (push-triple x y z cross-face-id (mod (inc cross-quintant) 5) max-row)))))))
