;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.math.mat2
  "Column-major 2x2 matrices as [m00 m01 m10 m11], the gl-matrix layout A5
  uses. The identity is the literal [1 0 0 1], so only the rotations need
  computing.")

(defn from-rotation [rad]
  (let [s (Math/sin rad)
        c (Math/cos rad)]
    [c s (- s) c]))
