(ns dodeca.crosscheck-test
  "dodeca against pya5 1.0.0 on the seeded points and grid disks
  scripts/crosscheck.py records: every cell bit for bit."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.core :as a5]
            [dodeca.fixture :as fx]))

(def crosscheck (fx/load-fixture "crosscheck/pya5.json"))

(deftest lon-lat->cell-agrees-with-pya5
  (let [cells (:cells crosscheck)
        mismatches (vec (keep (fn [[lon lat resolution expected]]
                                (let [actual (a5/cell->hex (a5/lon-lat->cell [lon lat] resolution))]
                                  (when (not= expected actual)
                                    [lon lat resolution expected actual])))
                              cells))]
    (is (= 100000 (count cells)))
    (is (empty? mismatches) (str (count mismatches) " mismatches, the first: " (take 5 mismatches)))))

(deftest grid-disk-agrees-with-pya5
  (let [disks (:disks crosscheck)
        mismatches (vec (keep (fn [[cell k expected]]
                                (let [actual (mapv a5/cell->hex (a5/grid-disk (a5/hex->cell cell) k))]
                                  (when (not= expected actual)
                                    [cell k expected actual])))
                              disks))]
    (is (= 3000 (count disks)))
    (is (empty? mismatches) (str (count mismatches) " mismatches, the first: " (take 3 mismatches)))))
