(ns dodeca.core.compaction-marker-test
  "A5 tests the compaction marker through its coverings; these pin the layout
  its header comment gives."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.core.compaction-marker :as m]
            [dodeca.core.hex :as hex]
            [dodeca.core.serialization :as s]
            [dodeca.fixture :as fx]))

(deftest marker-layout
  (is (= "f003000000000040" (hex/u64->hex (m/compaction-marker 3))))
  (doseq [r (range (inc s/max-resolution))]
    (let [marker (m/compaction-marker r)]
      (is (m/compaction-marker? marker) (str r))
      (is (= r (m/compaction-marker-resolution marker)) (str r))
      (is (not (s/valid-cell? marker)) (str r)))))

(deftest no-cell-is-a-marker
  (is (not (m/compaction-marker? s/world-cell)))
  (doseq [id (concat (fx/load-fixture "test-ids.json")
                     (:testIds (fx/load-fixture "serialization.json")))]
    (is (not (m/compaction-marker? (hex/hex->u64 id))) id))
  (is (not (m/compaction-marker? (bit-or (bit-shift-left 60 58) (bit-shift-left 31 48) 2r1000000)))))
