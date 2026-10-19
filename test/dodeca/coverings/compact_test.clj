(ns dodeca.coverings.compact-test
  "Ported from A5's tests/compact.test.ts. compact and uncompact are outside
  dodeca's scope, but grid-disk returns a covering, so its tests need them,
  and these fixtures pin the slot-run engine underneath."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.core.hex :as hex]
            [dodeca.core.serialization :as s]
            [dodeca.coverings.compact :as compact]
            [dodeca.coverings.resolution :refer [covering-resolution]]
            [dodeca.fixture :as fx]))

(def fixtures (fx/load-fixture "compact.json"))

(defn- cells [hexes] (mapv hex/hex->u64 hexes))

(deftest uncompact
  (doseq [{:keys [name input expectedCount expectedCells expectedResolution]} (:uncompact fixtures)]
    (let [input (cells input)
          result (compact/uncompact input)]
      (is (= expectedCount (count result)) name)
      (is (= (cells expectedCells) result) name)
      (is (= expectedResolution (covering-resolution input)) name)
      (is (every? #(= expectedResolution (s/get-resolution %)) result) name))))

(deftest compact
  (doseq [{:keys [name input expectedOutput]} (:compact fixtures)]
    (is (= (cells expectedOutput) (compact/compact (cells input))) name)))

(deftest round-trip
  (doseq [{:keys [name initialCells afterCompact expectedFinalCount resolution]} (:roundTrip fixtures)]
    (let [after (cells afterCompact)]
      (is (= after (compact/compact (cells initialCells))) name)
      (let [result (compact/uncompact after)]
        (is (= expectedFinalCount (count result)) name)
        (is (every? #(= resolution (s/get-resolution %)) result) name)))))
