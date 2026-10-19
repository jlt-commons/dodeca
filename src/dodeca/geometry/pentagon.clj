;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.geometry.pentagon
  "A pentagon as five [x y] vertices, counter-clockwise.

  A5's PentagonShape mutates in place; here every operation returns a new
  pentagon so the value can be shared, which is why clone is the identity."
  (:require [dodeca.math.vec2 :as vec2]))

;; How close (as a fraction of its length) to either end of p3->p4 a crossing
;; counts as touching only that endpoint
(def ^:private vertex-margin 1e-9)

(defn area
  "The signed shoelace sum, positive for counter-clockwise vertices."
  ^double [p]
  (let [n (count p)]
    (loop [i 0
           signed-area 0.0]
      (if (= i n)
        signed-area
        (let [[vx vy] (nth p i)
              [wx wy] (nth p (if (= (inc i) n) 0 (inc i)))]
          (recur (inc i) (+ signed-area (* (- (double wx) (double vx)) (+ (double wy) (double vy))))))))))

(defn shape
  "A pentagon's vertices, reversed if they are wound clockwise."
  [vertices]
  (if (>= (area vertices) 0)
    (vec vertices)
    (vec (reverse vertices))))

(defn winding-correct? [p]
  (>= (area p) 0))

(defn vertices [p] p)

(defn scale [p s]
  (mapv #(vec2/scale % s) p))

(defn rotate-180 [p]
  (mapv vec2/negate p))

(defn reflect-y
  "Reflect over the x-axis, reversing the winding order to match."
  [p]
  (vec (reverse (mapv (fn [[x y]] [x (- y)]) p))))

(defn translate [p translation]
  (mapv #(vec2/add % translation) p))

(defn transform [p m]
  (mapv #(vec2/transform-mat2 % m) p))

(defn transform2d [p m]
  (mapv #(vec2/transform-mat2d % m) p))

(defn get-center [p]
  (let [n (count p)]
    (reduce (fn [[sx sy] v] [(+ sx (/ (double (first v)) n)) (+ sy (/ (double (second v)) n))]) [0 0] p)))

(defn contains-point
  "1 when the point is inside, else a negative value proportional to how far
  outside the nearest edge it is."
  [p [px py]]
  (when-not (winding-correct? p)
    (throw (ex-info "Pentagon is not counter-clockwise" {:pentagon p})))
  (let [n (count p)
        px (double px)
        py (double py)]
    (loop [i 0
           d-max 1.0]
      (if (= i n)
        d-max
        (let [[v1x v1y] (nth p i)
              [v2x v2y] (nth p (if (= (inc i) n) 0 (inc i)))
              v1x (double v1x)
              v1y (double v1y)
              dx (- v1x (double v2x))
              dy (- v1y (double v2y))
              px (- px v1x)
              py (- py v1y)
              cross-product (- (* dx py) (* dy px))]
          (recur (inc i)
                 (if (< cross-product 0)
                   (min d-max (/ cross-product (Math/sqrt (+ (* px px) (* py py)))))
                   d-max)))))))

(defn- segments2d-intersect?
  "True iff the closed segment p1->p2 crosses p3->p4 away from p3 and p4."
  [p1 p2 p3 p4]
  (let [d1x (- (first p2) (first p1))
        d1y (- (second p2) (second p1))
        d2x (- (first p4) (first p3))
        d2y (- (second p4) (second p3))
        denom (- (* d1x d2y) (* d1y d2x))]
    ;; Parallel or degenerate when the sine between them is ~0 relative to the
    ;; segment lengths; an absolute threshold swallows small cells.
    (if (<= (* denom denom)
            (* 1e-24 (+ (* d1x d1x) (* d1y d1y)) (+ (* d2x d2x) (* d2y d2y))))
      false
      (let [dx (- (first p3) (first p1))
            dy (- (second p3) (second p1))
            t (/ (double (- (* dx d2y) (* dy d2x))) denom)
            u (/ (double (- (* dx d1y) (* dy d1x))) denom)]
        (and (>= t 0) (<= t 1) (> u vertex-margin) (< u (- 1 vertex-margin)))))))

(defn clip-segment
  "The part of segment a->b inside this convex pentagon as
  {:start :end :exit-edge-t}, or nil when the line misses it."
  [p a b]
  (let [n (count p)
        sx (- (first b) (first a))
        sy (- (second b) (second a))]
    (loop [i 0
           start ##-Inf
           end ##Inf
           exit-edge -1]
      (if (= i n)
        (if (or (> start end) (neg? exit-edge))
          nil
          (let [v1 (nth p exit-edge)
                v2 (nth p (mod (inc exit-edge) n))
                px (- (+ (first a) (* end sx)) (first v1))
                py (- (+ (second a) (* end sy)) (second v1))
                ex (- (first v2) (first v1))
                ey (- (second v2) (second v1))]
            {:start start
             :end end
             :exit-edge-t (/ (double (+ (* px ex) (* py ey))) (+ (* ex ex) (* ey ey)))}))
        (let [v1 (nth p i)
              v2 (nth p (mod (inc i) n))
              ex (- (first v1) (first v2))
              ey (- (second v1) (second v2))
              f (- (* ex (- (second a) (second v1)))
                   (* ey (- (first a) (first v1))))
              g (- (* ex sy) (* ey sx))]
          (cond
            (zero? g) (if (neg? f) nil (recur (inc i) start end exit-edge))
            (pos? g) (recur (inc i) (max start (/ (double (- f)) g)) end exit-edge)
            :else (let [t (/ (double (- f)) g)]
                    (if (< t end)
                      (recur (inc i) start t i)
                      (recur (inc i) start end exit-edge)))))))))

(defn intersects-segment?
  "True if either endpoint is inside, or any edge crosses the segment."
  [p a b]
  (if (or (pos? (contains-point p a)) (pos? (contains-point p b)))
    true
    (let [n (count p)]
      (boolean (some (fn [i]
                       (segments2d-intersect? a b (nth p i) (nth p (mod (inc i) n))))
                     (range n))))))

(defn split-edges
  "Each edge split into `segments` pieces, new vertices included."
  [p segments]
  (if (<= segments 1)
    p
    (let [n (count p)]
      (shape (vec (mapcat (fn [i]
                            (let [v1 (nth p i)
                                  v2 (nth p (mod (inc i) n))]
                              (cons v1
                                    (for [j (range 1 segments)]
                                      (vec2/lerp v1 v2 (/ (double j) segments))))))
                          (range n)))))))
