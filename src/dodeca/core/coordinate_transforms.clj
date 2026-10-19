;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.coordinate-transforms
  "Conversions between A5's coordinate systems: degrees and radians, face
  [x y] and polar [rho gamma], IJ, barycentric, Cartesian [x y z], spherical
  [theta phi] and [longitude latitude]."
  (:require [dodeca.core.pentagon :refer [basis basis-inverse]]
            [dodeca.math.ecma :as ecma]
            [dodeca.math.vec2 :as vec2]
            [dodeca.math.vec3 :as vec3]
            [dodeca.projections.authalic :as authalic]))

(defn deg->rad [deg]
  (* deg (/ Math/PI 180)))

(defn rad->deg [rad]
  (* rad (/ 180 Math/PI)))

(defn to-polar [[x y :as xy]]
  (let [rho (vec2/length xy)          ; Radial distance from face center
        gamma (Math/atan2 (double y) (double x))]  ; Azimuthal angle
    [rho gamma]))

(defn to-face [[rho gamma]]
  [(* rho (Math/cos gamma)) (* rho (Math/sin gamma))])

(defn face->ij [face]
  (vec2/transform-mat2 face basis-inverse))

(defn ij->face [ij]
  (vec2/transform-mat2 ij basis))

(defn face->barycentric
  "Face coordinates to barycentric coordinates in the face triangle."
  [[px py] [[p1x p1y] [p2x p2y] [p3x p3y]]]
  (let [d31x (- p1x p3x) d31y (- p1y p3y)
        d23x (- p3x p2x) d23y (- p3y p2y)
        d3px (- px p3x) d3py (- py p3y)
        det (- (* d23x d31y) (* d23y d31x))
        ;; Doubles, as in JS: integer inputs would make a ratio, and a zero det an infinity
        b0 (/ (double (- (* d23x d3py) (* d23y d3px))) det)
        b1 (/ (double (- (* d31x d3py) (* d31y d3px))) det)
        b2 (- 1 (+ b0 b1))]
    [b0 b1 b2]))

(defn barycentric->face
  "Barycentric coordinates in the face triangle to face coordinates."
  [[b0 b1 b2] [[p1x p1y] [p2x p2y] [p3x p3y]]]
  [(+ (+ (* b0 p1x) (* b1 p2x)) (* b2 p3x))
   (+ (+ (* b0 p1y) (* b1 p2y)) (* b2 p3y))])

(defn to-spherical [[x y z]]
  ;; Jolt's atan2 rejects two exact zeros, so the arguments are doubles, as in JS
  (let [theta (Math/atan2 (double y) (double x))
        ;; atan2 keeps full precision near the poles, where acos(z/r) loses half
        ;; of the digits carried (its derivative blows up as z/r -> ±1)
        rxy (Math/sqrt (+ (* x x) (* y y)))
        phi (Math/atan2 rxy (double z))]
    [theta phi]))

(defn to-cartesian [[theta phi]]
  (let [sin-phi (Math/sin phi)]
    [(* sin-phi (Math/cos theta))
     (* sin-phi (Math/sin theta))
     (Math/cos phi)]))

;; The offset longitude for the spherical coordinate system: the angle between
;; the Greenwich meridian and the vector between the centers of the first two
;; origins (dodecahedron face centers). It is chosen so that the majority of
;; the world's population, around 99.9% (and thus land mass), is located in the
;; first 8.5 dodecahedron faces, and thus comes first along the curve.
(def ^:private longitude-offset 93)

(defn from-lon-lat
  "[longitude latitude] in degrees to spherical [theta phi] in radians."
  [[longitude latitude]]
  (let [theta (deg->rad (+ longitude longitude-offset))
        geodetic-lat (deg->rad latitude)
        authalic-lat (authalic/forward geodetic-lat)
        phi (- (/ Math/PI 2) authalic-lat)]
    [theta phi]))

(defn normalize-longitude
  "Normalize a longitude value to the range [-180, 180)."
  [lon]
  (- (ecma/rem (+ (ecma/rem (+ lon 180) 360) 360) 360) 180))

(defn to-lon-lat
  "Spherical [theta phi] in radians to [longitude latitude] in degrees."
  [[theta phi]]
  (let [longitude (normalize-longitude (- (rad->deg theta) longitude-offset))
        authalic-lat (- (/ Math/PI 2) phi)
        geodetic-lat (authalic/inverse authalic-lat)]
    [longitude (rad->deg geodetic-lat)]))

(defn normalize-longitudes
  "Normalizes the longitudes of a contour of [longitude latitude] points to
  handle antimeridian crossing: each is moved within 180° of the contour's
  center."
  [contour]
  (let [center (vec3/normalize (reduce (fn [c p] (vec3/add c (to-cartesian (from-lon-lat p)))) [0 0 0] contour))
        [center-lon center-lat] (to-lon-lat (to-spherical center))
        ;; Near poles, use first point's longitude
        center-lon (if (or (> center-lat 89.99) (< center-lat -89.99)) (first (first contour)) center-lon)
        center-lon (normalize-longitude center-lon)]
    (mapv (fn [[longitude latitude]]
            ;; Adjust longitude to be closer to center
            (let [longitude (loop [l longitude] (if (> (- l center-lon) 180) (recur (- l 360)) l))
                  longitude (loop [l longitude] (if (< (- l center-lon) -180) (recur (+ l 360)) l))]
              [longitude latitude]))
          contour)))
