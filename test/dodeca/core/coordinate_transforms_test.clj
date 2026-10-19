(ns dodeca.core.coordinate-transforms-test
  "Ported from A5's tests/coordinate-transforms.test.ts. TEST_POINTS comes
  from A5's TypeScript test data via scripts/probe-projections.mjs, which also
  records A5's own results for a seeded set of points."
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.core.coordinate-transforms :as ct]
            [dodeca.fixture :as fx]
            [dodeca.matchers :refer [close-to? close-to-array?]]))

(def probe (fx/load-fixture "probes/projections.json"))

(def test-points-lon-lat
  [[0 0] [90 0] [180 0] [0 45] [0 -45] [-90 -45] [180 45] [90 45] [0 90] [0 -90] [123 45]])

(def test-triangle [[0 0] [1 0] [0 1]])

(deftest angle-conversions
  (is (= Math/PI (ct/deg->rad 180)))
  (is (= (/ Math/PI 2) (ct/deg->rad 90)))
  (is (zero? (ct/deg->rad 0)))
  (is (= 180.0 (ct/rad->deg Math/PI)))
  (is (= 90.0 (ct/rad->deg (/ Math/PI 2))))
  (is (zero? (ct/rad->deg 0))))

(deftest barycentric
  (testing "face -> barycentric -> face round-trips"
    (doseq [point (:testPoints probe)]
      (let [[b0 b1 b2 :as bary] (ct/face->barycentric point test-triangle)]
        (is (close-to-array? (ct/barycentric->face bary test-triangle) point 12) (str point))
        (is (close-to? (+ (+ b0 b1) b2) 1 12))
        (is (and (>= b0 0) (>= b1 0) (>= b2 0)) (str point)))))
  (testing "barycentric -> face -> barycentric round-trips"
    (doseq [bary [[0.043821975867140296 0.9561208684797726 0.00005715565308705983]
                  [0.5 0.3 0.2] [0.1 0.8 0.1] [0.33 0.33 0.34] [0.9 0.05 0.05] [0.001 0.999 0.0]]]
      (let [[r0 r1 r2 :as result] (ct/face->barycentric (ct/barycentric->face bary test-triangle) test-triangle)]
        (is (close-to-array? result bary 12))
        (is (close-to? (+ (+ r0 r1) r2) 1 12)))))
  (testing "vertices and edge midpoints"
    (doseq [[point expected] (map vector (concat test-triangle [[0.5 0] [0 0.5] [0.5 0.5]])
                                  [[1 0 0] [0 1 0] [0 0 1] [0.5 0.5 0] [0.5 0 0.5] [0 0.5 0.5]])]
      (let [bary (ct/face->barycentric point test-triangle)]
        (is (close-to-array? bary expected 12) (str point))
        (is (close-to-array? (ct/barycentric->face bary test-triangle) point 12) (str point))))))

(deftest coordinate-conversions
  (is (close-to-array? (ct/to-cartesian [0 0]) [0 0 1]))
  (is (close-to-array? (ct/to-cartesian [0 (/ Math/PI 2)]) [1 0 0]))
  (is (close-to-array? (ct/to-cartesian [(/ Math/PI 2) (/ Math/PI 2)]) [0 1 0]))
  (let [original [(/ Math/PI 4) (/ Math/PI 6)]]
    (is (close-to-array? (ct/to-spherical (ct/to-cartesian original)) original)))
  (testing "full precision for phi near the poles"
    (doseq [phi [1e-12 1e-9 1e-6 1e-3 (- Math/PI 1e-3) (- Math/PI 1e-6) (- Math/PI 1e-9)]]
      (let [[_ phi-out] (ct/to-spherical (ct/to-cartesian [0.5 phi]))
            dist-from-pole (min phi (- Math/PI phi))]
        (is (<= (Math/abs (- phi-out phi)) (* 1e-12 (max dist-from-pole 1e-3))) (str phi))))))

(deftest lon-lat-to-and-from-spherical
  (is (close-to-array? (ct/from-lon-lat [0 0]) [(ct/deg->rad 93) (/ Math/PI 2)]))
  (is (close-to-array? (ct/from-lon-lat [0 90]) [(ct/deg->rad 93) 0]))
  (is (close-to-array? (ct/from-lon-lat [0 -90]) [(ct/deg->rad 93) Math/PI]))
  (doseq [[lon lat] test-points-lon-lat]
    ;; 180 and -180 are equivalent longitudes (antimeridian)
    (is (close-to-array? (ct/to-lon-lat (ct/from-lon-lat [lon lat])) [(if (= lon 180) -180 lon) lat])
        (str [lon lat]))))

(deftest normalize-longitudes
  (let [contour [[0 0] [10 0] [10 10] [0 10] [0 0]]]
    (is (= contour (ct/normalize-longitudes contour))))
  (let [normalized (ct/normalize-longitudes [[-170 0] [-175 0] [-180 0] [175 0] [170 0]])]
    (is (close-to? (first (nth normalized 3)) -185))
    (is (close-to? (first (nth normalized 4)) -190))))

;; A5's own results for seeded points. The trigonometric functions come from
;; the platform's libm here and from V8's fdlibm port in A5, which differ in
;; the last bit now and then, so these hold to 1e-13 rather than exactly.
(def ^:private tolerance 13)

(defn- same?
  "Equal element for element as numbers: JSON spells an integral double as an
  integer."
  [expected actual]
  (and (= (count expected) (count actual)) (every? true? (map == expected actual))))

(deftest matches-a5-on-seeded-points
  (doseq [{:keys [lonLat spherical cartesian toSpherical toLonLat normalizeLongitude]} (:lonLat probe)]
    (is (same? spherical (ct/from-lon-lat lonLat)) (str lonLat))
    (is (close-to-array? (ct/to-cartesian spherical) cartesian tolerance) (str lonLat))
    ;; From this port's own Cartesian point: JSON drops the sign of a zero,
    ;; which turns atan2 half way round at the poles
    (is (close-to-array? (ct/to-spherical (ct/to-cartesian spherical)) toSpherical tolerance) (str lonLat))
    (is (close-to-array? (ct/to-lon-lat spherical) toLonLat tolerance) (str lonLat))
    (is (== normalizeLongitude (ct/normalize-longitude (* (first lonLat) 7.3))) (str lonLat)))
  (doseq [{:keys [face polar ij]} (:faces probe)]
    (is (close-to-array? (ct/to-polar face) polar tolerance) (str face))
    (is (same? ij (ct/face->ij face)) (str face))))
