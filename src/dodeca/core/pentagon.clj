;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.pentagon
  "The plane-tiling pentagon and the triangle lattice it is built from, scaled
  and rotated to match the unit sphere."
  (:require [dodeca.core.constants :as c]
            [dodeca.geometry.pentagon :as pentagon]
            [dodeca.math.vec2 :as vec2]))

;; Pentagon vertex angles
(def a-angle 72)
(def b-angle 127.94543761193603)
(def c-angle 108)
(def d-angle 82.29202980963508)
(def e-angle 149.7625318412527)

;; c and d are the intersections of vertex circles, not derived geometrically
(def ^:private unscaled
  [[0 0]
   [0 1]
   [0.7885966681787006 1.6149108024237764]
   [1.6171013659387945 1.054928690397459]
   [(Math/cos c/pi-over-10) (Math/sin c/pi-over-10)]])

(def ^:private edge-midpoint-d
  (* 2 (vec2/length (nth unscaled 2)) (Math/cos c/pi-over-5)))

;; The lattice grows along AC; rotate it parallel to the x-axis
(def ^:private basis-rotation
  (- c/pi-over-5 (Math/atan2 (second (nth unscaled 2)) (first (nth unscaled 2)))))

(def ^:private scale (/ (* 2 c/distance-to-edge) edge-midpoint-d))

(def ^:private scaled
  (mapv (fn [v] (vec2/rotate (vec2/scale v scale) [0 0] basis-rotation)) unscaled))

(def a (nth scaled 0))
(def b (nth scaled 1))
(def c (nth scaled 2))
(def d (nth scaled 3))
(def e (nth scaled 4))

(def pentagon-shape
  "Not equilateral, but it tiles the plane with five-fold rotational symmetry,
  so it can tile a regular pentagon."
  (pentagon/shape scaled))

(def ^:private bisector-angle (- (Math/atan2 (second c) (first c)) c/pi-over-5))

;; The triangle the lattice is really made of
(def u [0 0])
(def ^:private l (/ c/distance-to-edge (Math/cos c/pi-over-5)))

(def v-angle (+ bisector-angle c/pi-over-5))
(def v [(* l (Math/cos v-angle)) (* l (Math/sin v-angle))])

(def w-angle (- bisector-angle c/pi-over-5))
(def w [(* l (Math/cos w-angle)) (* l (Math/sin w-angle))])

(def triangle (pentagon/shape [u v w]))

(def basis
  "Column-major 2x2 of the lattice basis: the columns are v and w."
  [(first v) (second v) (first w) (second w)])

(def basis-inverse
  "Inlined 2x2 inverse, in gl-matrix's mat2.invert order so it is bit-identical."
  (let [det (/ 1 (- (* (first v) (second w)) (* (first w) (second v))))]
    [(* (second w) det)
     (* (- (second v)) det)
     (* (- (first w)) det)
     (* (first v) det)]))
