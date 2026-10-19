(ns dodeca.lattice.curve-test
  "Ported from A5's tests/lattice/curve.test.ts."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.fixture :as fx]
            [dodeca.lattice.index :as lattice]))

(def fixtures (fx/load-fixture "lattice/curve.json"))

(defn- label [{:keys [s resolution orientation]}]
  (str "s=" s " res=" resolution " ori=" orientation))

(deftest s->cell
  (doseq [{:keys [s resolution orientation x y z flavor] :as f} (:sToCell fixtures)]
    (let [{:keys [triple] :as cell} (lattice/s->cell s resolution (keyword orientation))]
      (is (= {:x x :y y :z z} triple) (label f))
      (is (= flavor (:flavor cell)) (label f)))))

(deftest s->triple
  (doseq [{:keys [s resolution orientation x y z] :as f} (:sToCell fixtures)]
    (is (= {:x x :y y :z z} (lattice/s->triple s resolution (keyword orientation))) (label f))))

(deftest triple-parity
  (doseq [{:keys [x y z parity] :as f} (:sToCell fixtures)]
    (is (= parity (lattice/triple-parity {:x x :y y :z z})) (label f))))

(deftest triple->s
  (doseq [{:keys [s resolution orientation x y z] :as f} (:sToCell fixtures)]
    (is (= s (lattice/triple->s {:x x :y y :z z} resolution (keyword orientation))) (label f))))

(deftest point->s
  (doseq [{:keys [i j resolution orientation s]} (:pointToS fixtures)]
    (is (= s (lattice/triple->s (lattice/round-to-triple [i j] resolution) resolution (keyword orientation)))
        (str "(" i "," j ") res=" resolution " ori=" orientation))))

(deftest triple-in-bounds
  (doseq [{:keys [x y z maxRow expected]} (:tripleInBounds fixtures)]
    (is (= expected (lattice/triple-in-bounds? {:x x :y y :z z} maxRow)) (str [x y z] " maxRow=" maxRow))))

(deftest triple-flavor-matches-the-descent
  ;; The closed form against the descent over all cells at res 0-6 (res 0
  ;; included: its single corner cell is flavor 2), two orientations
  (doseq [orientation [:uv :wu]
          res (range 7)
          :let [max-row (dec (bit-shift-left 1 res))
                failures (remove (fn [s]
                                   (let [cell (lattice/s->cell s res orientation)]
                                     (= (:flavor cell) (lattice/triple-flavor (:triple cell) max-row))))
                                 (range (bit-shift-left 1 (* 2 res))))]]
    (is (empty? failures) (str orientation " res " res))))
