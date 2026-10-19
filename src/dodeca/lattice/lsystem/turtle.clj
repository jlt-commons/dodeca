;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.lattice.lsystem.turtle
  "The turtle alphabet on the integer (a,b) triangular lattice (basis
  u=(√3/4,1/4), v=(0,1/2)). Draw symbols {E e S s U u D d T t} are unit
  segments; `+`/`-` are 60° turns. Each symbol also carries the 3 corners of
  the triangular cell it hosts. Everything here is exact integer: √3 only
  enters when (a,b) is later mapped to A5 triple coordinates (in index).

  A point is {:a :b}.")

(defn add [p q] {:a (+ (:a p) (:a q)) :b (+ (:b p) (:b q))})

(defn neg
  "180° rotation."
  [p]
  {:a (- (:a p)) :b (- (:b p))})

(defn- rot60
  "60° CCW, order 6."
  [{:keys [a b]}]
  {:a (- b) :b (+ a b)})

(defn rot-times [p n]
  (nth (iterate rot60 p) (mod n 6)))

(defn- AB [a b] {:a a :b b})

;; Step vector of each draw symbol at heading 0. Lowercase = same step, cell
;; hosted on the other side (see host-offsets).
(def ^:private base
  {\E (AB 4 0) \e (AB 4 0)
   \S (AB 4 -2) \s (AB 4 -2)
   \U (AB 0 2) \u (AB 0 2)
   \D (AB 0 -2) \d (AB 0 -2)
   \T (AB -4 0) \t (AB -4 0)})

(def draw (set (keys base)))

;; The 3 corner offsets (heading 0, from the segment start) of the cell each symbol hosts.
(def ^:private host-offsets
  {\E [(AB 0 0) (AB 4 0) (AB 4 -4)]
   \e [(AB 0 0) (AB 4 0) (AB 0 4)]
   \S [(AB 0 0) (AB 4 0) (AB 4 -4)]
   \s [(AB 4 -2) (AB 0 2) (AB 0 -2)]
   \U [(AB 0 2) (AB 0 -2) (AB 4 -2)]
   \u [(AB 0 0) (AB 0 4) (AB -4 4)]
   \D [(AB 0 2) (AB 0 -2) (AB 4 -2)]
   \d [(AB 0 0) (AB 0 -4) (AB -4 0)]
   \T [(AB 0 -4) (AB -4 0) (AB -4 -4)]
   \t [(AB -4 4) (AB 0 0) (AB 0 4)]})

(defn host-corners
  "The 3 (a,b) corners of the cell hosted by `sym`, drawn from `from` at `heading`."
  [sym from heading]
  (mapv (fn [o] (add from (rot-times o heading))) (get host-offsets sym)))

(defn host-sum
  "The corner SUM (= 3·centroid, an exact integer) of that cell."
  [sym from heading]
  (let [[p q r] (host-corners sym from heading)]
    {:a (+ (+ (:a p) (:a q)) (:a r)) :b (+ (+ (:b p) (:b q)) (:b r))}))

(defn walk
  "Walk a draw string (draw symbols + `+`/`-` turns) from (pos, heading).
  Returns the final turtle state {:pos :heading} and, as :draws, each draw
  symbol with the position and heading it was drawn from, before advancing:
  what A5's onDraw callback receives."
  [s pos heading]
  (reduce (fn [{p :pos h :heading :as state} ch]
            (cond
              (= ch \+) (assoc state :heading (mod (inc h) 6))
              (= ch \-) (assoc state :heading (mod (+ h 5) 6))
              (not (contains? draw ch)) state
              :else (-> state
                        (update :draws conj [ch p h])
                        (assoc :pos (add p (rot-times (get base ch) h))))))
          {:pos pos :heading (mod heading 6) :draws []}
          s))

(defn net-of
  "Net (a,b) displacement + net heading of a draw string, from origin at heading 0."
  [s]
  (let [end (walk s (AB 0 0) 0)]
    {:disp (:pos end) :d-heading (:heading end)}))
