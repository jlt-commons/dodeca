;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.cell-info
  "Counts, areas and edge lengths by resolution.

  A5 gives counts as a JS number, or a BigInt when asked; Clojure integers are
  exact either way, so these return a long, or a BigInt where a count passes
  2^63 (resolution 30). As a double each equals A5's number."
  (:require [dodeca.core.constants :refer [authalic-area-earth]]
            [dodeca.core.serialization :refer [first-curve-resolution]]))

(defn- pow4 [n]
  (reduce *' 1 (repeat n 4)))

(defn num-cells
  "The number of cells at a given resolution."
  [resolution]
  (cond
    (neg? resolution) 0
    (zero? resolution) 12
    :else (*' 60 (pow4 (dec resolution)))))

(defn num-children
  "The number of descendants a cell at `parent-resolution` (-1 for the world
  cell) has at `child-resolution`: 1 at the same resolution, 0 if coarser."
  [parent-resolution child-resolution]
  (cond
    (< child-resolution parent-resolution) 0
    (= child-resolution parent-resolution) 1
    ;; Between levels of constant aperture of 4, relation simplifies
    (>= parent-resolution first-curve-resolution) (pow4 (- child-resolution parent-resolution))
    :else (let [parent-count (num-cells parent-resolution)]
            (quot (num-cells child-resolution) (if (zero? parent-count) 1 parent-count)))))

(defn cell-area
  "The area of a cell at a given resolution in square metres."
  [resolution]
  (if (neg? resolution)
    authalic-area-earth
    (/ authalic-area-earth (double (num-cells resolution)))))

;; Mean cell edge length divided by sqrt(cellArea), measured exhaustively from
;; the cell boundaries. Resolution 0 cells (dodecahedron faces) have their own
;; geometry; from resolution 1 the pentagonal tiling refines self-similarly and
;; the ratio converges to ~0.8211, so a constant serves all higher resolutions.
(def ^:private edge-length-ratios [0.7131 0.8037 0.8164 0.8198 0.8208 0.821])
(def ^:private edge-length-ratio 0.8211)

(defn cell-edge-length-avg
  "The average edge length of a cell at a given resolution in metres.
  Individual edge lengths vary from the average by roughly ±10%, depending on
  the cell's shape and its position on the globe."
  [resolution]
  (let [resolution (max resolution 0)
        ratio (get edge-length-ratios resolution edge-length-ratio)]
    (* ratio (Math/sqrt (cell-area resolution)))))
