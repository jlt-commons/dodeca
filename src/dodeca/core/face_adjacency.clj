;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.face-adjacency
  "Which face and quintant lies across each face's base edges, and the map that
  carries cells across those seams."
  (:require [dodeca.core.constants :as c]
            [dodeca.core.pentagon :as pentagon]))

(def face-adjacency
  "face-adjacency[origin][quintant] = [adjacent-origin adjacent-quintant].
  Computed empirically from cell boundary vertex sharing at resolution 4."
  [[[1 2] [4 3] [5 4] [6 0] [11 1]]
   [[2 3] [4 4] [0 0] [11 0] [10 1]]
   [[9 2] [3 0] [4 0] [1 0] [10 0]]
   [[2 1] [9 1] [8 1] [5 1] [4 1]]
   [[2 2] [3 4] [5 0] [0 1] [1 1]]
   [[4 2] [3 3] [8 0] [6 1] [0 2]]
   [[0 3] [5 3] [8 4] [7 1] [11 2]]
   [[11 3] [6 3] [8 3] [9 4] [10 3]]
   [[5 2] [3 2] [9 0] [7 2] [6 2]]
   [[8 2] [3 1] [2 0] [10 4] [7 3]]
   [[2 4] [1 4] [11 4] [7 4] [9 3]]
   [[1 3] [0 4] [6 4] [7 0] [10 2]]])

(defn seam-transform
  "The map from a face's frame into its neighbour's frame across the base edge of
  `quintant`, as column-major [a b c d tx ty] taking (x, y) to
  (a x + c y + tx, b x + d y + ty).

  With R(k) the rotation of quintant k into place and q' the neighbour's
  quintant, it is p -> R(q') (BASIS (1, 1) - R(-q) p)."
  [origin-id quintant]
  (let [[_ adj-quintant] (get-in face-adjacency [origin-id quintant])
        angle (* c/two-pi-over-5 adj-quintant)
        cos (Math/cos angle)
        sin (Math/sin angle)
        ;; The linear part is R(q') R(-q) negated: a half-turn plus the change of quintant
        turn (- angle (* c/two-pi-over-5 quintant))
        ex (+ (nth pentagon/basis 0) (nth pentagon/basis 2))
        ey (+ (nth pentagon/basis 1) (nth pentagon/basis 3))]
    [(- (Math/cos turn))
     (- (Math/sin turn))
     (Math/sin turn)
     (- (Math/cos turn))
     (- (* cos ex) (* sin ey))
     (+ (* sin ex) (* cos ey))]))

(defn seam-triple
  "A cell's image across the seam at its quintant's base edge (`max-row` is its
  own), in the neighbour quintant's triples. The half-turn maps the lattice to
  itself, so the image is a cell: its pentagon is the cell's own turned half-way
  round, which flips the flavour's parity bit."
  [t max-row]
  {:x (- (- max-row) (:x t))
   :y (- (+ (* 2 max-row) 1) (:y t))
   :z (- (- max-row) (:z t))})

(defn walk-faces
  "Breadth-first walk over the 12 faces (the resolution 0 cells), adjacent across
  their edges. `seeds` are always expanded; every other face is visited once and
  expanded only if (expand face) is true. Stops after `max-rings`.

  Returns every face reached, seeds first."
  ([seeds expand] (walk-faces seeds expand ##Inf))
  ([seeds expand max-rings]
   (loop [reached (vec (distinct seeds))
          seen (set seeds)
          frontier (vec (distinct seeds))
          ring 0]
     (if (and (< ring max-rings) (seq frontier))
       (let [[seen reached next-frontier]
             (reduce (fn [[seen reached next-frontier] id]
                       (reduce (fn [[seen reached next-frontier] q]
                                 (let [face (get-in face-adjacency [id q 0])]
                                   (if (contains? seen face)
                                     [seen reached next-frontier]
                                     [(conj seen face)
                                      (conj reached face)
                                      (if (expand face) (conj next-frontier face) next-frontier)])))
                               [seen reached next-frontier]
                               (range 5)))
                     [seen reached []]
                     frontier)]
         (recur reached seen next-frontier (inc ring)))
       reached))))
