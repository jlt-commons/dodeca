;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.lattice.triple
  "Triple coordinates {:x :y :z} for the triangular grid underlying the
  pentagonal A5 grid. Neighbors differ by ±1 in exactly one coordinate while
  the other two stay constant. Triple coordinates are orientation-independent:
  the same geometric cell always has the same triple regardless of curve
  orientation."
  (:require [dodeca.lattice.lsystem.index :refer [triple->s-lattice]]))

(defn triple-parity
  "The parity of a triple (0 or 1), equal to x + y + z."
  [{:keys [x y z]}]
  (+ x y z))

;; The pentagon flavor is a CLOSED FORM of the triple. The A5 tiling is gyro
;; applied to the square grid R left when every lattice edge parallel to the
;; quintant's dodecahedron edge is deleted (A5 = g o^r D in Conway notation).
;; Bit 0 is the triangle's parity (which half of its rhombus it is); bit 1 is
;; the colour of its apex in the 2-colouring of R, coloured from a dodecahedron
;; vertex. The apex of either triangle in unit square (m, n) has colour
;; (m + n + maxRow + 1) & 1, and x + z = -(m + n). For maxRow + 1 even (every
;; resolution but 0) face centres and vertices share a colour, so bit 1 is just
;; (x + z) & 1; at resolution 0 they differ, which gives the corner cell
;; flavor 2.

(defn triple-flavor
  "The pentagon flavor (0-3) of a triple's cell: orientation-independent."
  [{:keys [x y z]} max-row]
  (bit-or (+ x y z) (bit-shift-left (bit-and (+ (+ (+ max-row 1) x) z) 1) 1)))

(defn triple-in-bounds?
  "Check if a triple is within valid quintant bounds."
  [{:keys [x y z]} max-row]
  (let [sum (+ x y z)]
    (and (or (= sum 0) (= sum 1))
         (let [limit (- y sum)]
           (and (<= x 0) (<= z 0) (>= y 0) (<= y max-row) (>= x (- limit)) (>= z (- limit)))))))

(defn triple->s
  "Convert triple coordinates to an s-value on the A5 (L-system) curve, or nil
  if the triple has invalid parity."
  ([t resolution] (triple->s t resolution :uv))
  ([t resolution orientation]
   (let [sum (triple-parity t)]
     (when (or (= sum 0) (= sum 1))
       (triple->s-lattice t resolution orientation)))))
