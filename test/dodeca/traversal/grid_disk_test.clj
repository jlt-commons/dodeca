(ns dodeca.traversal.grid-disk-test
  "Ported from A5's tests/traversal/grid-disk.test.ts."
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.core.compaction-marker :refer [compaction-marker?]]
            [dodeca.core.hex :refer [hex->u64 u64->hex]]
            [dodeca.coverings.compact :refer [uncompact]]
            [dodeca.fixture :as fx]
            [dodeca.traversal.grid-disk :as gd]))

(def cases (fx/load-fixture "traversal/grid-disk.json"))

(defn- sort-hex
  "Sort hex cell IDs by numeric value."
  [cells]
  (sort-by #(str (apply str (repeat (- 20 (count %)) \0)) %) cells))

(deftest grid-disk
  (doseq [{:keys [cellId k cells]} cases]
    (is (= (sort-hex cells) (sort-hex (map u64->hex (uncompact (gd/grid-disk (hex->u64 cellId) k)))))
        (str cellId " k=" k)))
  (testing "only the center cell for k=0"
    (let [result (gd/grid-disk (hex->u64 (:cellId (first cases))) 0)]
      (is (= 2 (count result)))
      (is (= (:cellId (first cases)) (u64->hex (first result))))
      (is (compaction-marker? (second result)))
      (is (= [(:cellId (first cases))] (map u64->hex (uncompact result)))))))

(deftest grid-disk-vertex
  (doseq [{:keys [cellId k cells extraVertexCells]} cases]
    (is (= (sort-hex (concat cells extraVertexCells))
           (sort-hex (map u64->hex (uncompact (gd/grid-disk-vertex (hex->u64 cellId) k)))))
        (str cellId " k=" k)))
  (testing "only the center cell for k=0"
    (let [result (gd/grid-disk-vertex (hex->u64 (:cellId (first cases))) 0)]
      (is (= 2 (count result)))
      (is (= (:cellId (first cases)) (u64->hex (first result))))
      (is (compaction-marker? (second result)))
      (is (= [(:cellId (first cases))] (map u64->hex (uncompact result)))))))
