(ns dodeca.core.cell-test
  "Ported from A5's tests/cell.test.ts, with the boundary-edge check of
  tests/cell-info.test.ts, which needs cell->boundary."
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.core.cell :as cell]
            [dodeca.core.cell-info :refer [cell-edge-length-avg]]
            [dodeca.core.constants :refer [authalic-radius-earth]]
            [dodeca.core.coordinate-transforms :refer [from-lon-lat]]
            [dodeca.core.hex :refer [hex->u64 u64->hex]]
            [dodeca.core.serialization :as s]
            [dodeca.fixture :as fx]
            [dodeca.matchers :refer [close-to?]]))

(def populated-places (fx/load-fixture "data/ne_50m_populated_places_nameonly.json"))
(def cell-to-lon-lat-fixtures (fx/load-fixture "cell-to-lonlat.json"))
(def sequence-fixtures (fx/load-fixture "lonlat-to-cell-sequence.json"))
(def serialization-fixtures (fx/load-fixture "serialization.json"))

(deftest world-cell
  (is (= 0 (cell/lon-lat->cell [0 0] -1)))
  (is (= [0 0] (cell/cell->lon-lat 0)))
  (is (= [] (cell/cell->boundary 0))))

(deftest antimeridian-cells
  (doseq [cell-id ["eb60000000000000" "2e00000000000000"]
          segments [1 10 :auto]]
    (let [longitudes (map first (cell/cell->boundary (hex->u64 cell-id) {:segments segments}))]
      (is (< (- (apply max longitudes) (apply min longitudes)) 180) (str cell-id " " segments)))))

(deftest cell-contains-the-original-point
  ;; Resolution 30 is skipped, as in A5
  (let [failures (for [feature (:features populated-places)
                       :let [lon-lat (vec (get-in feature [:geometry :coordinates]))]
                       resolution (range 1 s/max-resolution)
                       :let [cell-id (cell/lon-lat->cell lon-lat resolution)]
                       :when (neg? (cell/a5cell-contains-point (s/deserialize cell-id) (from-lon-lat lon-lat)))]
                   [(get-in feature [:properties :name]) lon-lat resolution (u64->hex cell-id)])]
    (is (empty? failures))))

(deftest cell-to-lon-lat
  (doseq [{:keys [cell_id center_lonlat input_lonlat resolution]} cell-to-lon-lat-fixtures]
    (let [[lon lat] (cell/cell->lon-lat (hex->u64 cell_id))]
      (is (and (>= lon -180) (<= lon 180) (>= lat -90) (<= lat 90)) cell_id)
      (is (close-to? lon (first center_lonlat) 10) cell_id)
      (is (close-to? lat (second center_lonlat) 10) cell_id))
    (testing "and the fixture's point lies in its cell"
      (is (= cell_id (u64->hex (cell/lon-lat->cell input_lonlat resolution))) (str input_lonlat " " resolution)))))

(deftest lon-lat-to-cell-sequence
  ;; The lookups are made in order: each must match a cold lookup of its point,
  ;; whatever the lookup before it was
  (let [{:keys [points sequences]} sequence-fixtures]
    (doseq [{:keys [resolution lookups cells]} sequences
            [i lookup expected] (map vector (range) lookups cells)]
      (is (= expected (u64->hex (cell/lon-lat->cell (nth points lookup) resolution)))
          (str "resolution " resolution ", lookup " i)))))

(def ^:private deg (/ Math/PI 180))

(defn- geodesic [[lon1 lat1] [lon2 lat2]]
  (let [lat1 (* lat1 deg)
        lat2 (* lat2 deg)
        h (+ (Math/pow (Math/sin (/ (- lat2 lat1) 2)) 2)
             (* (* (Math/cos lat1) (Math/cos lat2)) (Math/pow (Math/sin (/ (* (- lon2 lon1) deg) 2)) 2)))]
    (* (* 2 authalic-radius-earth) (Math/asin (Math/sqrt h)))))

(deftest boundary-edges-within-ten-percent-of-the-average
  (let [segments 10]
    (doseq [h (:testIds serialization-fixtures)]
      (let [cell-id (hex->u64 h)
            avg (cell-edge-length-avg (s/get-resolution cell-id))
            boundary (cell/cell->boundary cell-id {:closed-ring true :segments segments})
            num-edges (quot (dec (count boundary)) segments)]
        (doseq [e (range num-edges)]
          (let [length (reduce + (for [i (range segments)
                                       :let [idx (+ (* e segments) i)]]
                                   (geodesic (nth boundary idx) (nth boundary (inc idx)))))
                ratio (/ length avg)]
            (is (> ratio 0.9) (str "cell " h " edge " e))
            (is (< ratio 1.1) (str "cell " h " edge " e))))))))
