;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.math.vec2
  "The gl-matrix v3.4.3 double-precision 2-vectors A5 uses. A vector is a
  Clojure vector, so gl-matrix's `out` parameter is the return value."
  (:require [dodeca.math.hypot :refer [hypot]]))

(defn add [[ax ay] [bx by]]
  [(+ ax bx) (+ ay by)])

(defn scale [[ax ay] s]
  [(* ax s) (* ay s)])

(defn scale-and-add [[ax ay] [bx by] s]
  [(+ ax (* bx s)) (+ ay (* by s))])

(defn negate [[ax ay]]
  [(- ax) (- ay)])

(defn length [[ax ay]]
  (hypot ax ay))

(defn lerp [[ax ay] [bx by] t]
  [(+ ax (* t (- bx ax)))
   (+ ay (* t (- by ay)))])

(defn rotate
  "Rotate a around b by rad."
  [[ax ay] [bx by] rad]
  (let [p0 (- ax bx)
        p1 (- ay by)
        sin-c (Math/sin rad)
        cos-c (Math/cos rad)]
    [(+ (- (* p0 cos-c) (* p1 sin-c)) bx)
     (+ (+ (* p0 sin-c) (* p1 cos-c)) by)]))

(defn transform-mat2
  "m is column-major [m00 m01 m10 m11]."
  [[x y] [m00 m01 m10 m11]]
  [(+ (* m00 x) (* m10 y))
   (+ (* m01 x) (* m11 y))])

(defn transform-mat2d
  "m is column-major [m00 m01 m10 m11 tx ty]."
  [[x y] [m00 m01 m10 m11 tx ty]]
  [(+ (+ (* m00 x) (* m10 y)) tx)
   (+ (+ (* m01 x) (* m11 y)) ty)])
