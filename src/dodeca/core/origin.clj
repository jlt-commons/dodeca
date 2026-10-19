;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.origin
  "The twelve quintant origins, and the mapping between an origin's quintants
  and segments."
  (:require [dodeca.core.constants :as c]
            [dodeca.core.dodecahedron-quaternions :as dq]
            [dodeca.math.quat :as quat]))

(def counter-jump [:vu :uv :wv :wu :uw])
(def clockwise-jump [:wu :uw :vw :vu :uv])
(def clockwise-step [:wu :uw :vw :vu :uw])

(def ^:private quintant-orientations
  [clockwise-step  ; 0 Arctic
   counter-jump    ; 1 North America
   counter-jump    ; 2 South America
   clockwise-step  ; 3 North Atlantic & Western Europe & Africa
   counter-jump    ; 4 South Atlantic & Africa
   counter-jump    ; 5 Europe, Middle East & Central Africa
   counter-jump    ; 6 Indian Ocean
   clockwise-jump  ; 7 Asia
   clockwise-step  ; 8 Australia
   clockwise-jump  ; 9 North Pacific
   counter-jump    ; 10 South Pacific
   counter-jump])  ; 11 Antarctic

(def ^:private quintant-first [4 2 2 2 0 4 3 2 1 0 3 0])

(def origin-order [0 1 2 4 3 5 7 8 6 11 10 9])

(defn- add-origin
  "A5 pushes origins in generation order and treats the array index as the id;
  keying by id here keeps the two in step."
  [origins id axis angle q]
  (assoc origins id
        {:id id
         :axis axis
         :quat q
         :inverse-quat (quat/conjugate q)
         :angle angle
         :orientation (nth quintant-orientations id)
         :first-quintant (nth quintant-first id)}))

(defn- generate-origins []
  (reduce (fn [origins i]
            (let [alpha (* i c/two-pi-over-5)
                  alpha2 (+ alpha c/pi-over-5)]
              (-> origins
                  (add-origin (inc (* 2 i)) [alpha c/interhedral-angle] c/pi-over-5
                              (nth dq/quaternions (+ i 1)))
                  (add-origin (+ 2 (* 2 i)) [alpha2 (- Math/PI c/interhedral-angle)] c/pi-over-5
                              (nth dq/quaternions (+ 6 (mod (+ i 3) 5)))))))
          (add-origin {} 0 [0 0] 0 (nth dq/quaternions 0))
          (range 5)))

(defn- generate-south-pole [origins]
  (add-origin origins 11 [0 Math/PI] 0 (nth dq/quaternions 11)))

(def origins
  "Origins reordered to match the order of the Hilbert-ish curve, then
  renumbered 0-11 to match."
  (let [generated (generate-south-pole (generate-origins))]
    (vec (map-indexed (fn [i old-id]
                        (assoc (get generated old-id) :id i))
                      origin-order))))

(defn- haversine*
  "`haversine` given the sines of both colatitudes, which a search over the
  origins computes once rather than per origin: the same values, so the same
  result."
  ^double [^double theta ^double phi ^double sin-phi ^double theta2 ^double phi2 ^double sin-phi2]
  (let [a1 (Math/sin (/ (- phi2 phi) 2.0))
        a2 (Math/sin (/ (- theta2 theta) 2.0))]
    (+ (* a1 a1) (* (* (* a2 a2) sin-phi) sin-phi2))))

(defn haversine
  "Squared half-chord between two points given as [theta phi] and [theta2 phi2].
  Minimising this is the same as minimising great-circle distance, without the
  asin."
  [[theta phi] [theta2 phi2]]
  (let [phi (double phi)
        phi2 (double phi2)]
    (haversine* theta phi (Math/sin phi) theta2 phi2 (Math/sin phi2))))

(defn face-step
  "Direction of travel around a face: 1 counterclockwise, -1 clockwise."
  [origin]
  (let [layout (:orientation origin)]
    (if (or (= layout clockwise-jump) (= layout clockwise-step)) -1 1)))

(defn quintant->segment
  [quintant origin]
  (let [layout (:orientation origin)
        step (face-step origin)
        delta (mod (+ (- quintant (:first-quintant origin)) 5) 5)
        face-relative-quintant (mod (+ (* step delta) 5) 5)]
    {:segment (mod (+ (:first-quintant origin) face-relative-quintant) 5)
     :orientation (nth layout face-relative-quintant)}))

(defn segment->quintant
  [segment origin]
  (let [layout (:orientation origin)
        step (face-step origin)
        face-relative-quintant (mod (+ (- segment (:first-quintant origin)) 5) 5)]
    {:quintant (mod (+ (:first-quintant origin) (* step face-relative-quintant) 5) 5)
     :orientation (nth layout face-relative-quintant)}))

(def ^:private origin-axes
  "Each origin's axis [theta phi] with sin(phi)."
  (mapv (fn [{[theta phi] :axis}] [(double theta) (double phi) (Math/sin phi)]) origins))

(defn- origin-distances
  "The haversine from a point to each origin, in origin order."
  [[theta phi]]
  (let [theta (double theta)
        phi (double phi)
        sin-phi (Math/sin phi)]
    (mapv (fn [[theta2 phi2 sin-phi2]] (haversine* theta phi sin-phi theta2 phi2 sin-phi2)) origin-axes)))

(defn find-nearest-origins
  "The `count` origins nearest to a point, nearest first. Points on a seam may
  belong to a cell of the 2nd- or 3rd-nearest face."
  [point n]
  (let [distances (origin-distances point)]
    (vec (take n (sort-by (fn [origin] (nth distances (:id origin))) origins)))))

(defn find-nearest-origin
  [point]
  (let [distances (origin-distances point)]
    (loop [i 0
           min-distance ##Inf
           nearest 0]
      (if (= i 12)
        (nth origins nearest)
        (let [distance (double (nth distances i))]
          (if (< distance min-distance)
            (recur (inc i) distance i)
            (recur (inc i) min-distance nearest)))))))

(defn is-nearest-origin
  [point origin]
  (> (haversine point (:axis origin)) 0.49999999))
