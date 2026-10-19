;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.utils.vector
  "The two vector utilities A5 keeps in utils/vector: the scalar triple
  product and slerp."
  (:require [dodeca.math.vec3 :as vec3]))

(defn triple-product
  "A · (B × C), written out fully in the same operation order as cross
  followed by dot, so the results are bit-identical."
  [[a0 a1 a2] [b0 b1 b2] [c0 c1 c2]]
  (+ (* a0 (- (* b1 c2) (* b2 c1)))
     (* a1 (- (* b2 c0) (* b0 c2)))
     (* a2 (- (* b0 c1) (* b1 c0)))))

(defn precompute-slerp
  "gamma and sin(gamma) for a fixed (A, B) pair, so loops that slerp many
  times along the same arc don't re-run angle and sin."
  [A B]
  (let [gamma (vec3/angle A B)]
    {:gamma gamma, :sin-gamma (Math/sin gamma)}))

(defn slerp
  "Spherical linear interpolation between A and B at parameter t. Pass the
  context from `precompute-slerp` when slerping many t values along the same
  arc, to skip recomputing gamma and sin(gamma)."
  ([A B t]
   (let [gamma (vec3/angle A B)]
     (if (< gamma 1e-12)
       (vec3/lerp A B t)
       (slerp A B t {:gamma gamma, :sin-gamma (Math/sin gamma)}))))
  ([A B t {:keys [gamma sin-gamma]}]
   (if (< gamma 1e-12)
     (vec3/lerp A B t)
     (let [weight-a (/ (Math/sin (* (- 1 t) gamma)) sin-gamma)
           weight-b (/ (Math/sin (* t gamma)) sin-gamma)
           [ax ay az] A
           [bx by bz] B]
       [(+ (* weight-a ax) (* weight-b bx))
        (+ (* weight-a ay) (* weight-b by))
        (+ (* weight-a az) (* weight-b bz))]))))
