(ns dodeca.core.constants-test
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.core.constants :as c]
            [dodeca.fixture :as fx]))

(def tolerance 1e-12)

(defn- close? [expected actual]
  (<= (Math/abs (- (double expected) (double actual))) tolerance))

(deftest golden-ratio
  (let [f (:φ (fx/load-fixture "constants.json"))]
    (is (close? (:value f) c/φ))
    (is (close? (get-in f [:properties :goldenRatioSquared]) (* c/φ c/φ)))
    (is (close? (get-in f [:properties :goldenRatioPlusOne]) (+ c/φ 1)))
    (is (close? (get-in f [:properties :reciprocal]) (/ 1 c/φ)))))

(deftest angles
  (let [f (fx/load-fixture "constants.json")
        angle (fn [k] (get-in f [:angles k :value]))]
    (is (close? (angle :TWO_PI) c/two-pi))
    (is (close? (angle :TWO_PI_OVER_5) c/two-pi-over-5))
    (is (close? (angle :PI_OVER_5) c/pi-over-5))
    (is (close? (angle :PI_OVER_10) c/pi-over-10))))

(deftest dodecahedron-angles
  (let [f (fx/load-fixture "constants.json")
        angle (fn [k] (get-in f [:dodecahedronAngles k :value]))]
    (is (close? (angle :dihedralAngle) c/dihedral-angle))
    (is (close? (angle :interhedralAngle) c/interhedral-angle))
    (is (close? (angle :faceEdgeAngle) c/face-edge-angle))
    (is (close? (get-in f [:dodecahedronAngles :angleSum])
                (+ c/dihedral-angle c/interhedral-angle)))))

(deftest distances-and-radii
  (let [f (fx/load-fixture "constants.json")]
    (is (close? (get-in f [:distances :distanceToEdge :value]) c/distance-to-edge))
    (is (close? (get-in f [:distances :distanceToVertex :value]) c/distance-to-vertex))
    (is (close? (get-in f [:sphereRadii :Rinscribed :value]) c/r-inscribed))
    (is (close? (get-in f [:sphereRadii :Rmidedge :value]) c/r-midedge))
    (is (close? (get-in f [:sphereRadii :Rcircumscribed :value]) c/r-circumscribed))))

(deftest earth
  (is (= 6371007.2 c/authalic-radius-earth))
  (is (close? (* 4 Math/PI 6371007.2 6371007.2) c/authalic-area-earth)))

(deftest every-constant-is-finite-and-positive
  (let [f (fx/load-fixture "constants.json")]
    (doseq [{:keys [value]} (get-in f [:validationTests :finiteNumbers])]
      (is (not (Double/isInfinite value)))
      (is (not (Double/isNaN value))))
    (doseq [{:keys [value]} (get-in f [:validationTests :positiveConstants])]
      (is (pos? value)))))
