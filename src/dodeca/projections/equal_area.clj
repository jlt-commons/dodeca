;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.projections.equal-area
  "Equal area projection originally described by Snyder92 (AN EQUAL-AREA MAP
  PROJECTION FOR POLYHEDRAL GLOBES), with closed form equations due to
  Brenton R. S. Recht.

  The projection maps a point V within a spherical triangle ABC onto a planar
  point F (within a planar triangle), in an equal-area-preserving manner.

  The first point of the triangle (A) is known as the radiating vertex and
  the choice of this vertex subtly modifies how the projection behaves. All
  three choices yield an equal area projection, but the cusps vary.

  The transformation is done via an intermediate point P, obtained by
  intersecting the two great circles formed by A&V and B&C (hence why A is
  special). The equal-area transformation is then done by computing the ratio
  of areas between triangles ABP & ABC.

  The inverse follows the reverse procedure of obtaining P from the face
  triangle by inverting the equal-area transformation, before slerping
  between A&P to obtain V.

  A5's EqualAreaProjection holds the constants of one canonical triangle,
  which all of the dodecahedron's are congruent to; here they are the first
  argument of `forward` and `inverse`.

  A division that can meet a zero divisor (the point antipodal to A) has a
  double operand, so it gives JS's infinity or NaN rather than Jolt's (and
  the JVM's) ArithmeticException on boxed numbers."
  (:require [dodeca.core.coordinate-transforms :refer [face->barycentric barycentric->face]]
            [dodeca.geometry.spherical-polygon :refer [spherical-triangle-area]]
            [dodeca.math.mat2d :as mat2d]
            [dodeca.math.vec2 :as vec2]
            [dodeca.math.vec3 :as vec3]))

(defn compute-constants
  "The constants of a spherical triangle [A B C]: :a-dot-b, the canonical
  (\"even\") B-C orientation, :a-dot-c, the mirror (\"odd\") one, with B and C
  swapped, :alpha-transform, the affine transform from the alpha direction
  vector to P, :area-abc, and :volume-abc, the signed triple product
  A · (B × C)."
  [[A B C]]
  (let [BxC (vec3/cross B C)
        a-dot-b (vec3/dot A B)
        a-dot-c (vec3/dot A C)
        b-dot-c (vec3/dot B C)
        V (vec3/dot A BxC)
        P (+ a-dot-c b-dot-c)
        Q (+ a-dot-b 1)
        R (- (* a-dot-b b-dot-c) a-dot-c)
        F (- (* P P) (* Q Q))
        G (* (* 2 Q) R)
        alpha-transform (mat2d/from-values (- (* V V) F) (- G) (* (* -2 V) P) (* (* 2 V) Q) (+ (* V V) F) G)]
    {:volume-abc V
     :area-abc (spherical-triangle-area A B C)
     :a-dot-b a-dot-b
     :a-dot-c a-dot-c
     :alpha-transform alpha-transform}))

(defn forward
  "A spherical point to face coordinates."
  [{:keys [area-abc volume-abc]} V [A B C] face-triangle]
  ;; Compute point P, where great circles through A&V and B&C intersect
  (let [BxC (vec3/cross B C)
        volume-vbc (vec3/dot V BxC)
        P (vec3/scale-and-add (vec3/scale V volume-abc) A (- volume-vbc))
        D (vec3/length P)
        oo-d (if (> D 0) (/ 1 D) 1)
        P (vec3/scale P oo-d)
        ;; Obtain rho & alpha by ratio of areas
        area-abp (max 0 (spherical-triangle-area A B P))
        alpha (/ area-abp area-abc)
        rho (* (/ D volume-abc) (Math/sqrt (/ (double (+ 1 (vec3/dot A P))) (+ 1 (vec3/dot A V)))))
        ;; Construct barycentric triangle and map to face
        b [(- 1 rho) (* rho (- 1 alpha)) (* rho alpha)]]
    (barycentric->face b face-triangle)))

(defn inverse
  "Face coordinates to a spherical point."
  [{:keys [a-dot-b a-dot-c alpha-transform area-abc]} face-point face-triangle [A B C]]
  ;; Map from face to barycentric
  (let [[b0 b1 b2] (face->barycentric face-point face-triangle)
        threshold (- 1 1e-14)]
    (cond
      (> b0 threshold) A
      (> b1 threshold) B
      (> b2 threshold) C
      :else
      ;; Normalize odd (mirror-image) triangles to the canonical even orientation
      ;; by swapping B<->C and the matching weight b1<->b2, so alpha-transform is correct
      (let [face-a-dot-b (vec3/dot A B)
            odd (> (Math/abs (- face-a-dot-b a-dot-b)) (Math/abs (- face-a-dot-b a-dot-c)))
            B' (if odd C B)
            C' (if odd B C)
            b2 (if odd b1 b2)
            ;; Obtain rho & alpha
            rho (- 1 b0)
            alpha (* (/ b2 rho) area-abc)
            ;; Inverse to obtain point P (see forward)
            [w0 w1] (vec2/transform-mat2d [(Math/cos alpha) (Math/sin alpha)] alpha-transform)
            P (vec3/normalize (vec3/scale-and-add (vec3/scale B' w0) C' w1))
            ;; Compute weights for A & P
            s (vec3/dot A P)
            t (+ 1 (* (* rho rho) (- s 1)))
            weight-p (* rho (Math/sqrt (/ (double (+ 1 t)) (+ 1 s))))
            weight-a (- t (* s weight-p))]
        (vec3/scale-and-add (vec3/scale A weight-a) P weight-p)))))
