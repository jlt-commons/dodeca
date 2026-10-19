;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.lattice.curve
  "Public A5 space-filling curve: point -> triple, using the L-system curve
  (dodeca.lattice.lsystem). The s <-> cell mappings live in lsystem.index
  (s->cell / s->triple) and triple (triple->s)."
  (:require [dodeca.lattice.lsystem.tables :refer [pow2]]))

(defn round-to-triple
  "Locate the lattice triangle containing a fractional IJ point, as a triple.

  The triples tile the IJ plane as triangles: the unit square (m, n) =
  (floor(i), floor(j)) splits along the diagonal u+v = 1 into a lower
  triangle (the parity-0 cell (-n, m+n, -m), centroid (m+1/3, n+1/3)) and an
  upper triangle (the parity-1 cell (-n, m+n+1, -m), centroid (m+2/3,
  n+2/3)). Point location is two floors + one diagonal comparison. Points
  exactly on a triangle edge have no unique cell; the >= tie-break below is
  the fixed convention.

  The result is clamped into quintant bounds (m >= 0, n >= 0, m+n+parity <=
  maxRow, equivalent to triple-in-bounds?): a point slightly outside the
  quintant (as float noise can produce near quintant edges) must still map to
  a valid cell for the exact encode."
  [[i j] resolution]
  (let [max-row (dec (nth pow2 resolution))
        fm (Math/floor i)
        fn' (Math/floor j)
        parity (if (>= (+ (- i fm) (- j fn')) 1) 1 0)
        m (max 0 (long fm))
        n (max 0 (long fn'))
        [m n parity] (if (> (+ m n parity) max-row)
                       (if (> (+ m n) max-row)
                         (let [over (- (+ m n) max-row)
                               dm (min m over)]
                           [(- m dm) (- n (- over dm)) 0])
                         [m n 0])
                       [m n parity])]
    {:x (- n) :y (+ (+ m n) parity) :z (- m)}))
