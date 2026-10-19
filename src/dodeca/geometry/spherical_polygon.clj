;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.geometry.spherical-polygon
  "Spherical polygons as vectors of unit Cartesian [x y z] vertices. A5 uses
  Cartesian coordinates for all of these calculations for accuracy: [x y z]
  gives equal precision in all directions, unlike spherical coordinates.

  A5's SphericalPolygonShape wraps the vertices; here its methods take them
  directly."
  (:require [dodeca.math.ecma :as ecma]
            [dodeca.math.vec3 :as vec3]
            [dodeca.utils.vector :as v]))

(defn spherical-triangle-area
  "Signed area (spherical excess) of the spherical triangle (v1, v2, v3) on the
  unit sphere, in radians, by the Van Oosterom-Strackee formula: atan2 keeps
  full precision for tiny triangles and does not fold areas above π back into
  [-π, π]."
  [[v10 v11 v12 :as v1] [v20 v21 v22 :as v2] [v30 v31 v32 :as v3]]
  (let [norm (+ (+ (+ 1
                      (+ (+ (* v10 v20) (* v11 v21)) (* v12 v22)))
                   (+ (+ (* v20 v30) (* v21 v31)) (* v22 v32)))
                (+ (+ (* v30 v10) (* v31 v11)) (* v32 v12)))]
    (* 2 (Math/atan2 (double (v/triple-product v1 v2 v3)) (double norm)))))

(defn point-in-spherical-polygon?
  "Spherical point-in-polygon via signed-angle summation. Works for concave
  polygons (unlike `contains-point`, which assumes convex \"necessary
  strike\")."
  [[p0 p1 p2] vertices]
  (let [n (count vertices)
        angle-sum (reduce
                   (fn [angle-sum i]
                     (let [[a0 a1 a2] (nth vertices i)
                           [b0 b1 b2] (nth vertices (mod (inc i) n))
                           dot-pa (+ (+ (* p0 a0) (* p1 a1)) (* p2 a2))
                           dot-pb (+ (+ (* p0 b0) (* p1 b1)) (* p2 b2))
                           apx (- a0 (* dot-pa p0))
                           apy (- a1 (* dot-pa p1))
                           apz (- a2 (* dot-pa p2))
                           bpx (- b0 (* dot-pb p0))
                           bpy (- b1 (* dot-pb p1))
                           bpz (- b2 (* dot-pb p2))
                           cx (- (* apy bpz) (* apz bpy))
                           cy (- (* apz bpx) (* apx bpz))
                           cz (- (* apx bpy) (* apy bpx))]
                       (+ angle-sum
                          (Math/atan2 (double (+ (+ (* cx p0) (* cy p1)) (* cz p2)))
                                      (double (+ (+ (* apx bpx) (* apy bpy)) (* apz bpz)))))))
                   0
                   (range n))]
    (> (Math/abs (double angle-sum)) Math/PI)))

(defn- centroid
  "The normalised sum of the vertices."
  [vertices]
  (vec3/normalize (reduce vec3/add [0 0 0] vertices)))

(defn ring-winding-sign
  "Ring winding direction: 1 for CCW (interior to the left of edge direction),
  -1 for CW. Sums (v_i × v_{i+1}) · centroid across the ring."
  [ring-vecs]
  (let [c (centroid ring-vecs)
        n (count ring-vecs)
        sum (reduce (fn [sum i]
                      (+ sum (v/triple-product c (nth ring-vecs i) (nth ring-vecs (mod (inc i) n)))))
                    0
                    (range n))]
    (if (pos? sum) 1 -1)))

(defn ring-segment-normals
  "Great-circle plane normals for every segment of the ring."
  [ring-vecs]
  (let [n (count ring-vecs)]
    (mapv (fn [i] (vec3/cross (nth ring-vecs i) (nth ring-vecs (mod (inc i) n)))) (range n))))

(defn slerp
  "Interpolates along the boundary of the polygon: t = 1.5 is the midpoint
  between the 2nd and 3rd vertices."
  [vertices t]
  (let [n (count vertices)
        f (ecma/rem t 1)
        i (long (Math/floor (ecma/rem t n)))
        j (mod (inc i) n)]
    (v/slerp (nth vertices i) (nth vertices j) f)))

(defn get-boundary
  "A boundary of the polygon with n-segments points per edge, closed by
  repeating the first point unless `closed-ring` is false."
  ([vertices] (get-boundary vertices 1 true))
  ([vertices n-segments] (get-boundary vertices n-segments true))
  ([vertices n-segments closed-ring]
   (let [points (mapv (fn [s] (slerp vertices (/ (double s) n-segments)))
                      (range (* (count vertices) n-segments)))]
     (if closed-ring (conj points (first points)) points))))

(defn get-transformed-vertices
  "The vertex at index t, with the vectors from it to the vertex after (VA)
  and before (VB)."
  [vertices t]
  (let [n (count vertices)
        i (long (Math/floor (ecma/rem t n)))
        j (mod (inc i) n)
        k (mod (dec (+ i n)) n)
        V (nth vertices i)]
    [V (vec3/sub (nth vertices j) V) (vec3/sub (nth vertices k) V)]))

(defn contains-point
  "Positive when the point is inside every arc, 0 on an edge, and negative
  outside, the further from 0 the further from the nearest arc.

  An adaption of 'Locating a point on a spherical surface relative to a
  spherical polygon', using only the condition of 'necessary strike'."
  [vertices point]
  (reduce
   (fn [theta-delta-min i]
     ;; Transform point and neighboring vertices into coordinate system centered on vertex
     (let [[V VA VB] (get-transformed-vertices vertices i)
           ;; Normalize to obtain unit direction vectors
           VP (vec3/normalize (vec3/sub point V))
           VA (vec3/normalize VA)
           VB (vec3/normalize VB)
           ;; Cross products will point away from the center of the sphere when
           ;; point P is within arc formed by VA and VB
           cross-ap (vec3/cross VA VP)
           cross-pb (vec3/cross VP VB)
           ;; Dot product will be positive when point P is within arc formed by
           ;; VA and VB. Its magnitude is the sine of the angle between the two
           ;; vectors, which is the same as the angle for small angles.
           sin-ap (vec3/dot V cross-ap)
           sin-pb (vec3/dot V cross-pb)]
       ;; The minimum finds the arc where the point is closest to being outside
       (min theta-delta-min sin-ap sin-pb)))
   ##Inf
   (range (count vertices))))

(defn get-area
  "The area of the spherical polygon, in radians, from a fan of triangles."
  [vertices]
  (case (count vertices)
    (0 1 2) 0
    3 (spherical-triangle-area (nth vertices 0) (nth vertices 1) (nth vertices 2))
    (let [center (centroid vertices)
          n (count vertices)]
      ;; Sum fan of triangles around center
      (reduce (fn [area i]
                (let [tri-area (spherical-triangle-area center (nth vertices i) (nth vertices (mod (inc i) n)))]
                  (if (Double/isNaN tri-area) area (+ area tri-area))))
              0
              (range n)))))
