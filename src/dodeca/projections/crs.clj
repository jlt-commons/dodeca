;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.projections.crs
  "The Coordinate Reference System (CRS) of the dodecahedron is a set of 62
  vertices:
  - 12 face centers
  - 20 vertices
  - 30 edge midpoints

  The vertices are used as a rigid frame of reference for the dodecahedron in
  the dodecahedron projection. By constructing them once, we avoid
  recalculating them and can be sure of their correctness. A5's CRS class has
  a single instance; here it is the `vertices` vector."
  (:require [dodeca.core.constants :refer [distance-to-edge distance-to-vertex]]
            [dodeca.core.coordinate-transforms :refer [to-cartesian]]
            [dodeca.core.origin :refer [origins]]
            [dodeca.math.vec3 :as vec3]))

(defn- add
  "The vertices with the normalised new vertex, unless one is within 1e-5 of it."
  [vertices new-vertex]
  (let [normalized (vec3/normalize new-vertex)]
    (if (some #(< (vec3/distance normalized %) 1e-5) vertices)
      vertices
      (conj vertices normalized))))

(defn- add-face-centers [vertices]
  (reduce (fn [vs origin] (add vs (to-cartesian (:axis origin)))) vertices origins))

(defn- add-ring
  "One point per face corner (odd) or edge (even) multiple of π/5, at polar
  angle phi."
  [vertices phi odd]
  (reduce (fn [vs [origin i]]
            (let [theta (/ (* (if odd (+ (* 2 i) 1) (* 2 i)) Math/PI) 5)
                  v (to-cartesian [(+ theta (:angle origin)) phi])]
              (add vs (vec3/transform-quat v (:quat origin)))))
          vertices
          (for [origin origins i (range 5)] [origin i])))

(defn- add-vertices [vertices]
  (add-ring vertices (Math/atan distance-to-vertex) true))

(defn- add-midpoints [vertices]
  (add-ring vertices (Math/atan distance-to-edge) false))

(def vertices
  (let [vs (-> [] add-face-centers add-vertices add-midpoints)]
    (when-not (= 62 (count vs))
      (throw (ex-info "Failed to construct CRS: vertices length is not 62" {:count (count vs)})))
    vs))

(defn get-canonical-triangle
  "A canonical spherical face triangle (face center, edge midpoint, vertex) of
  the dodecahedron, taken from origin 0's CRS vertices. All face triangles
  used by the dodecahedron projection are congruent and consistently wound
  with this one, so it serves as the fixed source of the equal-area shape
  constants.

  The indices rely on the construction order: vertices[0] is origin 0's face
  center, vertices[12] its first corner (after the 12 centers) and
  vertices[32] its first edge midpoint (after the 20 corners)."
  []
  [(nth vertices 0) (nth vertices 32) (nth vertices 12)])

(defn get-vertex [point]
  (or (first (filter #(< (vec3/distance point %) 1e-5) vertices))
      (throw (ex-info "Failed to find vertex in CRS" {:point point}))))
