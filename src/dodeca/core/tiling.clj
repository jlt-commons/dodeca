;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.tiling
  "Cell pentagons in face coordinates: placing the base pentagon by a cell's
  triple and flavor, and the quintant and face outlines."
  (:require [dodeca.core.constants :refer [two-pi-over-5]]
            [dodeca.core.pentagon :refer [basis pentagon-shape triangle v]]
            [dodeca.geometry.pentagon :as pentagon]
            [dodeca.math.ecma :as ecma]
            [dodeca.math.mat2 :as mat2]
            [dodeca.math.vec2 :as vec2]))

(def ^:private triangle-mode false)

;; Transforms for each pentagon in the primitive unit
(def ^:private quintant-rotations
  (mapv (fn [quintant] (mat2/from-rotation (* two-pi-over-5 quintant))) (range 5)))

(defn- oriented
  "The base pentagon under a flavor's orientation ops: bit 0 a 180° rotation,
  bit 1 a Y reflection."
  [p flavor]
  (cond-> p
    (pos? (bit-and flavor 1)) pentagon/rotate-180
    (pos? (bit-and flavor 2)) pentagon/reflect-y))

;; Center of the base pentagon under each flavor's orientation ops. The vertex
;; mean is linear, so an oriented pentagon's center is the transformed base
;; center: no need to construct the five vertices when only the center is
;; wanted (see get-pentagon-center).
(def ^:private flavor-centers
  (mapv (fn [flavor] (pentagon/get-center (oriented pentagon-shape flavor))) (range 4)))

;; The base pentagon under each flavor's orientation ops
(def ^:private flavor-pentagons
  (mapv (fn [flavor] (oriented pentagon-shape flavor)) (range 4)))

(defn- translation
  "BASIS · ref, ref = (x + y, -x) in IJ, plus (0, 1) for the rotated flavors."
  [{:keys [x y]} flavor]
  (vec2/transform-mat2 [(+ x y) (+ (- x) (bit-and flavor 1))] basis))

(defn cell-margin-scaled
  "Signed containment margin of a point in the pentagon of (triple, flavor)
  (> 0 iff strictly inside; the most-violated-edge cross product otherwise),
  tested in the SCALED quintant-0 frame (face coords rotated into quintant 0
  and scaled by 2^resolution, the frame `spherical->cell` works in). In this
  frame the cell's pentagon is the flavor-oriented base pentagon translated by
  BASIS·(x+y, -x+(flavor&1)), so the test needs no curve decode, no
  re-projection, and, pentagons being unit-size here, stays well-conditioned
  at every resolution."
  [px py x y flavor]
  (let [rx (+ x y)
        ry (+ (- x) (bit-and flavor 1))
        tx (+ (* (nth basis 0) rx) (* (nth basis 2) ry))
        ty (+ (* (nth basis 1) rx) (* (nth basis 3) ry))
        pent (nth flavor-pentagons flavor)]
    (loop [i 0
           margin ##Inf]
      (if (= i 5)
        margin
        (let [[p1x p1y] (nth pent i)
              [p2x p2y] (nth pent (if (= i 4) 0 (inc i)))
              v1x (+ p1x tx)
              v1y (+ p1y ty)
              v2x (+ p2x tx)
              v2y (+ p2y ty)
              ;; (v1 - v2) × (p - v1): < 0 => strictly outside this edge
              cross (- (* (- v1x v2x) (- py v1y)) (* (- v1y v2y) (- px v1x)))]
          (recur (inc i) (if (< cross margin) cross margin)))))))

(defn get-pentagon-vertices
  "The pentagon of a cell, from its resolution (curve level), quintant
  (0-4), triple and flavor (0-3).

  A cell's pentagon is one of exactly four orientations of the base pentagon
  (the Cairo-like metatile): flavor bit 0 is a 180° rotation, bit 1 a Y
  reflection. The oriented pentagon sits at the triple-derived lattice point
  ref = (x+y, -x) in IJ, shifted by one j unit for the rotated flavors."
  ([resolution quintant triple flavor]
   (get-pentagon-vertices resolution quintant triple flavor triangle-mode))
  ([resolution quintant triple flavor triangle-mode]
   (-> (oriented (if triangle-mode triangle pentagon-shape) flavor)
       ;; Position within quintant: ref(triple), plus (0, 1) for the rotated flavors
       (pentagon/translate (translation triple flavor))
       (pentagon/scale (/ 1 (Math/pow 2 resolution)))
       (pentagon/transform (nth quintant-rotations quintant)))))

(defn get-pentagon-center
  "The center of a cell's pentagon, without constructing the pentagon: O(1)
  via the precomputed flavor centers. Equivalent to
  `(pentagon/get-center (get-pentagon-vertices ...))` up to float
  associativity."
  [resolution quintant triple flavor]
  (let [[cx cy] (nth flavor-centers flavor)
        [tx ty] (translation triple flavor)
        scale (Math/pow 2 resolution)]
    (vec2/transform-mat2 [(/ (+ cx tx) scale) (/ (+ cy ty) scale)]
                         (nth quintant-rotations quintant))))

(defn get-quintant-vertices [quintant]
  (pentagon/transform triangle (nth quintant-rotations quintant)))

(defn get-face-vertices []
  ;; Reversed to obtain the correct winding order
  (pentagon/shape (vec (reverse (mapv #(vec2/transform-mat2 v %) quintant-rotations)))))

(defn get-quintant-polar [[_ gamma]]
  (long (ecma/rem (+ (ecma/round (/ gamma two-pi-over-5)) 5) 5)))
