(ns dodeca.projections.crs-test
  "Ported from A5's tests/projections/crs.test.ts."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.fixture :as fx]
            [dodeca.math.vec3 :as vec3]
            [dodeca.matchers :refer [close-to? close-to-array?]]
            [dodeca.projections.crs :as crs]))

(def expected-vertices (fx/load-fixture "crs-vertices.json"))

(deftest sixty-two-vertices
  (is (= 62 (count crs/vertices))))

(deftest matches-expected-vertices
  (is (= (count expected-vertices) (count crs/vertices)))
  (doseq [[i vertex expected] (map vector (range) crs/vertices expected-vertices)]
    (is (close-to-array? vertex expected) (str "vertex " i))))

(deftest non-vertex-throws
  (is (= "Failed to find vertex in CRS"
         (try (crs/get-vertex [1 0 0]) nil (catch Exception e (ex-message e))))))

(deftest unit-vertices
  (doseq [vertex crs/vertices]
    (is (close-to? (vec3/length vertex) 1 15))))
