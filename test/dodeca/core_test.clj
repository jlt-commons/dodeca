(ns dodeca.core-test
  "The public API: one check per function, against A5's fixtures."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.core :as a5]
            [dodeca.fixture :as fx]))

(def cell-info (fx/load-fixture "cell-info.json"))
(def cell-to-lon-lat (fx/load-fixture "cell-to-lonlat.json"))
(def grid-disk-cases (fx/load-fixture "traversal/grid-disk.json"))

(deftest indexing
  (doseq [{:keys [input_lonlat resolution cell_id center_lonlat]} (take 10 cell-to-lon-lat)]
    (let [cell (a5/lon-lat->cell input_lonlat resolution)]
      (is (= cell_id (a5/cell->hex cell)))
      (is (= cell (a5/hex->cell cell_id)))
      (is (< (Math/abs (- (first (a5/cell->lon-lat cell)) (first center_lonlat))) 1e-10))
      (is (= 6 (count (a5/cell->boundary cell {:segments 1}))))
      (is (= 5 (count (a5/cell->boundary cell {:segments 1 :closed-ring false})))))))

(deftest hierarchy
  (is (= 30 a5/max-resolution))
  (is (= 0 a5/world-cell))
  (is (= 12 (count (a5/res0-cells))))
  (is (every? #(= a5/world-cell (a5/cell->parent %)) (a5/res0-cells)))
  (let [cell (a5/hex->cell "8708000000000000")]
    (is (a5/valid-cell? cell))
    (is (= 4 (a5/resolution cell)))
    (is (= cell (a5/cell->parent (first (a5/cell->children cell)))))
    (is (= 16 (count (a5/cell->children cell 6))))
    (is (= 2 (a5/resolution (a5/cell->parent cell 2))))))

(deftest cell-info-matches
  (doseq [{:keys [resolution countBigInt]} (:numCells cell-info)]
    (is (= countBigInt (str (a5/num-cells resolution)))))
  (doseq [{:keys [parentResolution childResolution numChildren]} (:numChildren cell-info)]
    (is (== numChildren (a5/num-children parentResolution childResolution))))
  (doseq [{:keys [resolution areaM2]} (:cellArea cell-info)]
    (is (= areaM2 (a5/cell-area resolution))))
  (doseq [{:keys [resolution lengthM]} (:cellEdgeLengthAvg cell-info)]
    (is (= lengthM (a5/cell-edge-length-avg resolution)))))

(deftest compaction-and-grid-disk
  (doseq [{:keys [cellId k cells]} (take 6 grid-disk-cases)]
    (let [disk (a5/grid-disk (a5/hex->cell cellId) k)]
      (is (a5/compaction-marker? (peek disk)))
      (is (= (set cells) (set (map a5/cell->hex (a5/uncompact disk)))))
      (is (= disk (a5/compact (a5/uncompact disk))))
      (is (<= (count cells) (count (a5/uncompact (a5/grid-disk-vertex (a5/hex->cell cellId) k))))))))
