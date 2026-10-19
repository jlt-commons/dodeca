(ns dodeca.lattice.compat-test
  "Ported from A5's tests/lattice/compat.test.ts."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.fixture :as fx]
            [dodeca.lattice.index :as lattice]))

(def fixtures (fx/load-fixture "lattice/compat.json"))

(defn- label [{:keys [s resolution orientation]}]
  (str "s=" s " res=" resolution " ori=" orientation))

(deftest compat-s->cell
  (doseq [{:keys [s resolution orientation x y z flavor] :as f} (:sToCell fixtures)]
    (is (= {:triple {:x x :y y :z z} :flavor flavor} (lattice/compat-s->cell s resolution (keyword orientation)))
        (label f))
    (is (= {:x x :y y :z z} (lattice/compat-s->triple s resolution (keyword orientation))) (label f))
    (is (= s (lattice/compat-triple->s {:x x :y y :z z} resolution (keyword orientation))) (label f))))

(deftest compat-ij->s
  (doseq [{:keys [i j resolution orientation s]} (:IJToS fixtures)]
    (is (= s (lattice/compat-ij->s [i j] resolution (keyword orientation)))
        (str "(" i "," j ") res=" resolution " ori=" orientation))))
