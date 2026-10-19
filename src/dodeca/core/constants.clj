;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.constants
  "The dodecahedron's constants, in A5's names and A5's order of operations.

  The golden ratio keeps its Greek name as A5 spells it.")

(def φ
  (/ (+ 1 (Math/sqrt 5)) 2))

(def two-pi
  (* 2 Math/PI))

(def two-pi-over-5
  (/ (* 2 Math/PI) 5))

(def pi-over-5
  (/ Math/PI 5))

(def pi-over-10
  (/ Math/PI 10))

(def dihedral-angle
  "Angle between pentagon faces, 116.565°."
  (* 2 (Math/atan φ)))

(def interhedral-angle
  "180° minus the dihedral angle, 63.435°."
  (- Math/PI dihedral-angle))

(def face-edge-angle
  "58.2825°, the angle from a face's centre to an edge's midpoint."
  (+ (* -0.5 Math/PI) (Math/acos (/ -1 (Math/sqrt (- 3 φ))))))

(def distance-to-edge
  (- φ 1))

(def distance-to-vertex
  (* 2 (- 2 φ)))

(def r-inscribed
  "Radius of the dodecahedron's inscribed sphere."
  1)

(def r-midedge
  "Radius of the sphere touching the dodecahedron's edge midpoints."
  (Math/sqrt (- 3 φ)))

(def r-circumscribed
  "Radius of the dodecahedron's circumscribed sphere."
  (/ (* (Math/sqrt 3) r-midedge) φ))

(def authalic-radius-earth
  "Authalic radius of Earth, in metres."
  6371007.2)

(def authalic-area-earth
  "Authalic surface area of Earth, in square metres."
  (* 4 Math/PI authalic-radius-earth authalic-radius-earth))
