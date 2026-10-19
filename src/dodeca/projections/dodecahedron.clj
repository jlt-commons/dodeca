;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.projections.dodecahedron
  "The dodecahedron projection: a gnomonic projection onto the face, then the
  equal-area projection of the face triangle the point lies in. A5's
  DodecahedronProjection caches its 30 face triangles and 240 spherical
  triangles as they are first used; here they are computed once, up front."
  (:require [dodeca.core.constants :refer [distance-to-edge interhedral-angle pi-over-5 two-pi-over-5]]
            [dodeca.core.coordinate-transforms :refer [to-cartesian to-spherical to-face to-polar]]
            [dodeca.core.origin :refer [origins]]
            [dodeca.core.tiling :refer [get-quintant-vertices]]
            [dodeca.math.ecma :as ecma]
            [dodeca.math.vec2 :as vec2]
            [dodeca.math.vec3 :as vec3]
            [dodeca.projections.crs :as crs]
            [dodeca.projections.equal-area :as equal-area]
            [dodeca.projections.gnomonic :as gnomonic]))

(def ^:private equal-area-constants
  (equal-area/compute-constants (crs/get-canonical-triangle)))

(defn normalize-gamma
  "Normalizes gamma to the range [-π/5, π/5]: the azimuthal angle from the
  triangle bisector."
  [gamma]
  (let [segment (/ gamma two-pi-over-5)
        s-center (ecma/round segment)
        s-offset (- segment s-center)]
    (* s-offset two-pi-over-5)))

(defn- should-reflect?
  "True if the point is beyond the edge of the dodecahedron face. In the
  standard case the face and spherical triangle can be used directly. In the
  reflected case the face triangle is squashed to unproject correctly onto
  the neighboring dodecahedron face."
  [[rho gamma]]
  (> (first (to-face [rho (normalize-gamma gamma)])) distance-to-edge))

(defn- get-face-triangle-index
  "The index (0-9) of the face triangle a polar coordinate belongs to."
  [[_ gamma]]
  (mod (+ (long (Math/floor (/ gamma pi-over-5))) 10) 10))

(defn- face-triangle*
  "The face triangle with that index: 3 vertices in counter-clockwise order."
  [face-triangle-index]
  (let [quintant (mod (quot (inc face-triangle-index) 2) 5)
        [v-center v-corner1 v-corner2] (get-quintant-vertices quintant)
        v-edge-midpoint (vec2/lerp v-corner1 v-corner2 0.5)]
    ;; Sign of gamma determines which triangle we want to use, and thus vertex
    ;; order. Center & midpoint compared to DGGAL's implementation are swapped,
    ;; as this is a dodecahedron, rather than an icosahedron.
    (if (even? face-triangle-index)
      [v-center v-edge-midpoint v-corner1]
      [v-center v-corner2 v-edge-midpoint])))

(defn- reflected-face-triangle* [face-triangle-index squashed]
  ;; First obtain ordinary unreflected triangle
  (let [[A B C] (face-triangle* face-triangle-index)
        ;; Reflect dodecahedron center (A) across edge (BC)
        midpoint (if (even? face-triangle-index) B C)
        ;; Squashing is important. A squashed triangle when unprojected will
        ;; yield the correct spherical triangle.
        A (vec2/scale-and-add (vec2/negate A) midpoint
                              (if squashed (+ 1 (/ 1 (Math/cos interhedral-angle))) 2))]
    ;; Swap midpoint and corner to maintain correct vertex order
    [A C B]))

;; By index: 0-9 plain, 10-19 reflected, 20-29 reflected and squashed
(def ^:private face-triangles
  (vec (concat (map face-triangle* (range 10))
               (map #(reflected-face-triangle* % false) (range 10))
               (map #(reflected-face-triangle* % true) (range 10)))))

(defn- get-face-triangle [face-triangle-index reflected squashed]
  (nth face-triangles (cond-> face-triangle-index reflected (+ (if squashed 20 10)))))

(defn- spherical-triangle* [face-triangle-index origin-id reflected]
  (let [origin (nth origins origin-id)]
    (mapv (fn [face]
            (let [[rho gamma] (to-polar face)
                  rotated (to-cartesian (gnomonic/inverse [rho (+ gamma (:angle origin))]))]
              (crs/get-vertex (vec3/transform-quat rotated (:quat origin)))))
          (get-face-triangle face-triangle-index reflected true))))

;; By index 10 * origin-id + face-triangle-index, plus 120 when reflected
(def ^:private spherical-triangles
  (vec (for [reflected [false true]
             origin-id (range 12)
             face-triangle-index (range 10)]
         (spherical-triangle* face-triangle-index origin-id reflected))))

(defn- get-spherical-triangle [face-triangle-index origin-id reflected]
  (nth spherical-triangles (cond-> (+ (* 10 origin-id) face-triangle-index) reflected (+ 120))))

(defn forward-cartesian
  "`forward` for a Cartesian unit vector: skips the to-cartesian round trip
  when the caller already has the Cartesian form."
  [unprojected origin-id]
  (let [origin (nth origins origin-id)
        ;; Transform back origin space
        out (vec3/transform-quat unprojected (:inverse-quat origin))
        ;; Unproject gnomonically to polar coordinates in origin space
        [rho gamma] (gnomonic/forward (to-spherical out))
        ;; Rotate around face axis to remove origin rotation
        polar [rho (- gamma (:angle origin))]
        face-triangle-index (get-face-triangle-index polar)
        reflect (should-reflect? polar)]
    (equal-area/forward equal-area-constants
                        unprojected
                        (get-spherical-triangle face-triangle-index origin-id reflect)
                        (get-face-triangle face-triangle-index reflect false))))

(defn forward
  "Spherical [theta phi] to face coordinates [x y] on the face of `origin-id`."
  [spherical origin-id]
  (forward-cartesian (to-cartesian spherical) origin-id))

(defn inverse
  "Face coordinates [x y] on the face of `origin-id` to spherical [theta phi]."
  [face origin-id]
  (let [polar (to-polar face)
        face-triangle-index (get-face-triangle-index polar)
        reflect (should-reflect? polar)]
    (to-spherical (equal-area/inverse equal-area-constants
                                      face
                                      (get-face-triangle face-triangle-index reflect false)
                                      (get-spherical-triangle face-triangle-index origin-id reflect)))))

(defn spherical-triangle
  "The spherical triangle of a face triangle on an origin's face, as the
  projection uses it."
  [face-triangle-index origin-id reflected]
  (get-spherical-triangle face-triangle-index origin-id reflected))
