;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.dodecahedron-quaternions
  "The twelve face-centre quaternions of the dodecahedron.

  A5 computes these from exact trigonometry rather than composing rotations:
  the expressions below are the simplifications of those trig terms, and
  computing them directly is what keeps the quaternions consistent with each
  other.")

(def ^:private sqrt5 (Math/sqrt 5))
(def ^:private inv-sqrt5 (Math/sqrt 0.2))

(def ^:private sin-alpha (Math/sqrt (/ (- 1 inv-sqrt5) 2)))
(def ^:private cos-alpha (Math/sqrt (/ (+ 1 inv-sqrt5) 2)))

(def ^:private a 0.5)
(def ^:private b (Math/sqrt (/ (- 2.5 sqrt5) 10)))
(def ^:private c (Math/sqrt (/ (+ 2.5 sqrt5) 10)))
(def ^:private d (Math/sqrt (/ (+ 1 inv-sqrt5) 8)))
(def ^:private e (Math/sqrt (/ (- 1 inv-sqrt5) 8)))
(def ^:private f (Math/sqrt (/ (- 3 sqrt5) 8)))
(def ^:private g (Math/sqrt (/ (+ 3 sqrt5) 8)))

(def ^:private face-centers
  "Face centres projected onto the z=0 plane: the pole, the first pentagon
  ring, the second ring, and the opposite pole."
  [[0 0]
   [sin-alpha 0]
   [b a]
   [(- d) f]
   [(- d) (- f)]
   [b (- a)]
   [(- cos-alpha) 0]
   [(- e) (- g)]
   [c (- a)]
   [c a]
   [(- e) g]
   [0 0]])

(def ^:private axes
  (mapv (fn [[x y]] [(- y) x]) face-centers))

(def quaternions
  "Index 0 is the north pole, 1-5 the first ring, 6-10 the second, 11 the south
  pole. The w component is the half-angle of the rotation from the pole."
  (vec (map-indexed (fn [i [ax ay]]
                      (cond
                        (zero? i) [0 0 0 1]
                        (= 11 i) [0 -1 0 0]
                        :else [ax ay 0 (if (< i 6) cos-alpha sin-alpha)]))
                    axes)))
