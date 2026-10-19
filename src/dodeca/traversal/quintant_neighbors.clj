;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.traversal.quintant-neighbors
  (:require [dodeca.lattice.index :refer [s->cell triple->s triple-in-bounds?]]
            [dodeca.traversal.neighbors :refer [neighbor-deltas]]))

(defn get-cell-neighbors
  "Neighbor finding via triple coordinates and pentagon flavor, within one
  quintant: the neighbors' s-values, ascending.

  Triple coordinates are orientation-independent: the same geometric cell
  always has the same triple coords regardless of curve orientation. Only the
  s-value changes between orientations, so neighbors are found in triple space
  and converted back to the requested orientation. With :edge-only, only the
  5 edge-sharing neighbors; otherwise the 2 vertex-only neighbors too."
  ([s resolution] (get-cell-neighbors s resolution :uv nil))
  ([s resolution orientation] (get-cell-neighbors s resolution orientation nil))
  ([s resolution orientation {:keys [edge-only]}]
   (let [{{:keys [x y z]} :triple flavor :flavor} (s->cell s resolution orientation)
         max-row (dec (bit-shift-left 1 resolution))
         deltas (get-in neighbor-deltas [flavor (if edge-only :edge :all)])]
     (vec (sort (for [d deltas
                      :let [neighbor {:x (+ x (:x d)) :y (+ y (:y d)) :z (+ z (:z d))}]
                      :when (triple-in-bounds? neighbor max-row)]
                  (triple->s neighbor resolution orientation)))))))
