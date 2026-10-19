;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.math.vec3
  "The gl-matrix v3.4.3 double-precision 3-vectors A5 uses. A vector is a
  Clojure vector, so gl-matrix's `out` parameter is the return value."
  (:require [dodeca.math.hypot :refer [hypot]]))

(defn add [[ax ay az] [bx by bz]]
  [(+ ax bx) (+ ay by) (+ az bz)])

(defn subtract [[ax ay az] [bx by bz]]
  [(- ax bx) (- ay by) (- az bz)])

(def sub subtract)

(defn scale [[ax ay az] s]
  [(* ax s) (* ay s) (* az s)])

(defn scale-and-add [[ax ay az] [bx by bz] s]
  [(+ ax (* bx s)) (+ ay (* by s)) (+ az (* bz s))])

(defn length [[ax ay az]]
  (hypot ax ay az))

(defn distance [[ax ay az] [bx by bz]]
  (hypot (- bx ax) (- by ay) (- bz az)))

(defn normalize
  "A zero vector normalises to zero, as gl-matrix leaves it: the length stays
  the squared length, so every component is multiplied by zero."
  [[ax ay az]]
  (let [len (+ (* ax ax) (* ay ay) (* az az))
        len (if (pos? len) (/ 1 (Math/sqrt len)) len)]
    [(* ax len) (* ay len) (* az len)]))

(defn dot [[ax ay az] [bx by bz]]
  (+ (* ax bx) (* ay by) (* az bz)))

(defn cross [[ax ay az] [bx by bz]]
  [(- (* ay bz) (* az by))
   (- (* az bx) (* ax bz))
   (- (* ax by) (* ay bx))])

(defn lerp [[ax ay az] [bx by bz] t]
  [(+ ax (* t (- bx ax)))
   (+ ay (* t (- by ay)))
   (+ az (* t (- bz az)))])

(defn transform-quat
  "Rotate a by the quaternion q, in gl-matrix's operation order."
  [[x y z] [qx qy qz qw]]
  (let [uvx (- (* qy z) (* qz y))
        uvy (- (* qz x) (* qx z))
        uvz (- (* qx y) (* qy x))
        uuvx (- (* qy uvz) (* qz uvy))
        uuvy (- (* qz uvx) (* qx uvz))
        uuvz (- (* qx uvy) (* qy uvx))
        w2 (* qw 2)
        uvx (* uvx w2)
        uvy (* uvy w2)
        uvz (* uvz w2)
        uuvx (* uuvx 2)
        uuvy (* uuvy 2)
        uuvz (* uuvz 2)]
    [(+ (+ x uvx) uuvx)
     (+ (+ y uvy) uuvy)
     (+ (+ z uvz) uuvz)]))

(defn angle
  "The angle between a and b, by the numerically stable half-angle form."
  [[ax ay az] [bx by bz]]
  (let [dx (- ax bx)
        dy (- ay by)
        dz (- az bz)
        sx (+ ax bx)
        sy (+ ay by)
        sz (+ az bz)]
    (* 2 (Math/atan2 (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))
                     (Math/sqrt (+ (* sx sx) (* sy sy) (* sz sz)))))))
