;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.math.ecma
  "ECMAScript's Math.round and % on doubles, where Jolt's own differ.

  Jolt's Math/round is floor(x + 0.5), which rounds 2^52 + 1 up; Math.round
  returns the nearest integer, ties toward +∞. Jolt's rem on doubles goes
  through a quotient and can be off for large ones (1e17 rem 360 gives 288,
  not 280); % is the exact fmod."
  (:refer-clojure :exclude [rem]))

(defn round
  "Math.round: the nearest integer as a double, ties toward +∞."
  [x]
  (let [x (double x)
        f (Math/floor x)]
    (if (>= (- x f) 0.5) (+ f 1.0) f)))

(defn rem
  "x % y on doubles: the exact remainder of truncating division, with the
  sign of x. Subtracts the largest y * 2^k that fits until less than y is
  left; each subtraction is exact (Sterbenz), so the result is."
  [x y]
  (let [x (double x)
        ax (Math/abs x)
        ay (Math/abs (double y))]
    (cond
      (or (Double/isNaN x) (Double/isNaN ay) (Double/isInfinite x) (zero? ay)) ##NaN
      (< ax ay) x
      :else (loop [r ax]
              (if (< r ay)
                (if (neg? x) (- r) r)
                (recur (- r (loop [d ay]
                              (let [d2 (* d 2.0)]
                                (if (<= d2 r) (recur d2) d))))))))))
