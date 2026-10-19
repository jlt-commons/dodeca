;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.traversal.neighbors
  "The neighbors of a cell, in triple space, are a fixed function of its
  pentagon flavor: pentagons tile edge-to-edge, so the arrangement around a
  pentagon is forced. Every cell has exactly 5 edge-sharing and 2 vertex-only
  neighbors, at the triple deltas below. No validation is needed: each delta
  IS a neighbor (bounds permitting).

  Derived geometrically (shared pentagon vertices) and verified exhaustively
  over all interior cells at res 4-5, all orientations, zero conflicts. The
  flavor-1/3 lists are the flavor-0/2 lists negated (they are the
  180°-rotated shapes).")

(defn- D [x y z] {:x x :y y :z z})

(def neighbor-deltas
  "By flavor: the 5 :edge-sharing and 2 :vertex-only neighbor deltas, and
  :all, edge ++ vertex, spelled out so this stays a pure data table."
  [{:edge   [(D 0 0 1) (D 0 1 -1) (D 0 1 0) (D 1 -1 0) (D 1 0 0)]
    :vertex [(D 1 -1 1) (D 1 1 -1)]
    :all    [(D 0 0 1) (D 0 1 -1) (D 0 1 0) (D 1 -1 0) (D 1 0 0) (D 1 -1 1) (D 1 1 -1)]}
   ;; flavor 1 (= flavor 0 rotated 180°: deltas negated)
   {:edge   [(D 0 0 -1) (D 0 -1 1) (D 0 -1 0) (D -1 1 0) (D -1 0 0)]
    :vertex [(D -1 1 -1) (D -1 -1 1)]
    :all    [(D 0 0 -1) (D 0 -1 1) (D 0 -1 0) (D -1 1 0) (D -1 0 0) (D -1 1 -1) (D -1 -1 1)]}
   ;; flavor 2
   {:edge   [(D -1 1 0) (D 0 -1 1) (D 0 0 1) (D 0 1 0) (D 1 0 0)]
    :vertex [(D -1 1 1) (D 1 -1 1)]
    :all    [(D -1 1 0) (D 0 -1 1) (D 0 0 1) (D 0 1 0) (D 1 0 0) (D -1 1 1) (D 1 -1 1)]}
   ;; flavor 3 (= flavor 2 rotated 180°: deltas negated)
   {:edge   [(D 1 -1 0) (D 0 1 -1) (D 0 0 -1) (D 0 -1 0) (D -1 0 0)]
    :vertex [(D 1 -1 -1) (D -1 1 -1)]
    :all    [(D 1 -1 0) (D 0 1 -1) (D 0 0 -1) (D 0 -1 0) (D -1 0 0) (D 1 -1 -1) (D -1 1 -1)]}])
