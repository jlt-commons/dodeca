(ns dodeca.geometry.spherical-polygon-test
  "Ported from A5's tests/geometry/spherical-polygon.test.ts and
  spherical-polygon-primitives.test.ts."
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.fixture :as fx]
            [dodeca.geometry.spherical-polygon :as sp]
            [dodeca.math.vec3 :as vec3]
            [dodeca.matchers :refer [close-to? close-to-array?]]))

(def primitives (fx/load-fixture "geometry/spherical-polygon-primitives.json"))
(def fixtures (fx/load-fixture "geometry/spherical-polygon.json"))

(def ^:private deg-to-rad (/ Math/PI 180))

(defn- ll->vec [[lon lat]]
  (let [lat (* lat deg-to-rad)
        lon (* lon deg-to-rad)
        cos-lat (Math/cos lat)]
    [(* cos-lat (Math/cos lon)) (* cos-lat (Math/sin lon)) (Math/sin lat)]))

(deftest point-in-spherical-polygon
  (doseq [{:keys [name ring points]} (:pointInSphericalPolygon primitives)
          :let [ring-vecs (mapv ll->vec ring)]
          {:keys [vec inside lonLat]} points]
    (is (= inside (sp/point-in-spherical-polygon? vec ring-vecs)) (str name " " lonLat))))

(deftest ring-winding-sign
  (doseq [{:keys [name ring sign]} (:ringWindingSign primitives)]
    (is (= sign (sp/ring-winding-sign (mapv ll->vec ring))) name)))

(deftest get-boundary
  (doseq [fixture fixtures
          n [1 2 3]]
    (let [boundary (sp/get-boundary (:vertices fixture) n true)
          expected (get fixture (keyword (str "boundary" n)))]
      (is (= (count expected) (count boundary)))
      (doseq [[point e] (map vector boundary expected)]
        (is (close-to-array? point e 6))))))

(deftest slerp
  (doseq [fixture fixtures
          {:keys [t result]} (:slerpTests fixture)]
    (let [actual (sp/slerp (:vertices fixture) t)]
      (is (close-to-array? actual result 6) (str t))
      (is (< (Math/abs (- (vec3/length actual) 1)) 1e-10)))))

(deftest contains-point
  (doseq [fixture fixtures
          {:keys [point result]} (:containsPointTests fixture)]
    (is (close-to? (sp/contains-point (:vertices fixture) point) result 6) (str point))))

(deftest get-area
  (doseq [fixture fixtures]
    (let [area (sp/get-area (:vertices fixture))]
      (is (close-to? area (:area fixture) 6))
      (is (pos? (Math/abs (double area))))
      (is (<= (Math/abs (double area)) (* 2 Math/PI)))))
  (testing "degenerate polygons"
    (is (= 0 (sp/get-area [])))
    (is (= 0 (sp/get-area [[1 0 0]])))
    (is (= 0 (sp/get-area [[1 0 0] [0 1 0]])))))

(deftest triangle-area-agrees-with-the-fan
  (doseq [fixture fixtures
          :let [[a b c] (:vertices fixture)]]
    (is (= (sp/spherical-triangle-area a b c) (sp/get-area [a b c])))))
