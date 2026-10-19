(ns dodeca.core.cell-info-test
  "Ported from A5's tests/cell-info.test.ts. The ±10% edge-length check needs
  cell->boundary and lives in dodeca.core.cell-test."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.core.cell-info :as info]
            [dodeca.fixture :as fx]))

(def fixtures (fx/load-fixture "cell-info.json"))

(deftest num-cells
  (doseq [{:keys [resolution count countBigInt]} (:numCells fixtures)]
    ;; `count` is a JS number, exact as a double; `countBigInt` is the exact integer
    (is (= (double count) (double (info/num-cells resolution))) (str resolution))
    (is (= countBigInt (str (info/num-cells resolution))) (str resolution))))

(deftest num-cells-below-zero
  (is (= 0 (info/num-cells -1))))

(deftest num-children
  (doseq [{:keys [parentResolution childResolution numChildren]} (:numChildren fixtures)]
    (is (== numChildren (info/num-children parentResolution childResolution))
        (str parentResolution " -> " childResolution))))

(deftest cell-area
  (doseq [{:keys [resolution areaM2]} (:cellArea fixtures)]
    (is (= areaM2 (info/cell-area resolution)) (str resolution))))

(deftest cell-edge-length-avg
  (doseq [{:keys [resolution lengthM]} (:cellEdgeLengthAvg fixtures)]
    (is (= lengthM (info/cell-edge-length-avg resolution)) (str resolution))))
