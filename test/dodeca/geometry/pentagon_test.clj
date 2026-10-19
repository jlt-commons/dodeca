(ns dodeca.geometry.pentagon-test
  "Pentagon geometry and the core lattice pentagon, checked against A5's own
  output via scripts/probe-pentagon.mjs."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [dodeca.core.pentagon :as core-pentagon]
            [dodeca.geometry.pentagon :as pentagon])
  (:import [java.io File]))

(def tolerance 1e-12)

(def probe
  (json/read-str (slurp (File. "test/fixtures/probes/pentagon.json")) :key-fn keyword))

(defn- close? [a b]
  (or (and (= a b))
      (< (Math/abs (- (double a) (double b))) tolerance)))

(defn- vec-close? [a b]
  (and (= (count a) (count b)) (every? true? (map close? a b))))

(defn- vertices-close? [actual expected]
  (and (= (count actual) (count expected))
       (every? true? (map vec-close? actual expected))))

(defn- same-shape? [actual expected]
  (and (close? (pentagon/area actual) (:area expected))
       (vertices-close? actual (:vertices expected))
       (vec-close? (pentagon/get-center actual) (:center expected))))

(defn- ->pentagon [name] (get-in probe [:shapes (keyword name)]))

(deftest core-pentagon-constants-match-a5
  (let [{:keys [A B C D E a b c d e u v w V BASIS BASIS_INVERSE PENTAGON TRIANGLE]} (:corePentagon probe)]
    (testing "vertex angles are exact"
      (is (= A core-pentagon/a-angle))
      (is (= B core-pentagon/b-angle))
      (is (= C core-pentagon/c-angle))
      (is (= D core-pentagon/d-angle))
      (is (= E core-pentagon/e-angle)))
    (testing "vertices"
      (is (vec-close? a core-pentagon/a))
      (is (vec-close? b core-pentagon/b))
      (is (vec-close? c core-pentagon/c))
      (is (vec-close? d core-pentagon/d))
      (is (vec-close? e core-pentagon/e)))
    (testing "triangle"
      (is (vec-close? u core-pentagon/u))
      (is (vec-close? v core-pentagon/v))
      (is (vec-close? w core-pentagon/w))
      (is (close? V core-pentagon/v-angle)))
    (testing "lattice basis"
      (is (vec-close? BASIS core-pentagon/basis))
      (is (vec-close? BASIS_INVERSE core-pentagon/basis-inverse)))
    (testing "shapes"
      (is (vertices-close? core-pentagon/pentagon-shape PENTAGON))
      (is (vertices-close? core-pentagon/triangle TRIANGLE)))))

(deftest pentagon-shape-matches-a5
  (doseq [name ["PENTAGON" "TRIANGLE" "synthetic0" "synthetic1" "synthetic2"]]
    (testing name
      (let [expected (->pentagon name)]
        (is (same-shape? (pentagon/shape (:vertices expected)) expected))))))

(deftest operations-match-a5
  (doseq [name ["PENTAGON" "TRIANGLE" "synthetic0" "synthetic1" "synthetic2"]]
    (testing name
      (let [expected (->pentagon name)
            p (pentagon/shape (:vertices expected))]
        (testing "translate"
          (is (same-shape? (pentagon/translate p [0.3 -0.7]) (:translate expected))))
        (testing "scale"
          (is (same-shape? (pentagon/scale p 2.5) (:scale expected))))
        (testing "rotate180"
          (is (same-shape? (pentagon/rotate-180 p) (:rotate180 expected))))
        (testing "reflectY"
          (is (same-shape? (pentagon/reflect-y p) (:reflectY expected))))
        (testing "transform"
          (is (same-shape? (pentagon/transform p [2 0 0 2]) (:transform expected))))
        (testing "splitEdges"
          (is (same-shape? (pentagon/split-edges p 2) (:splitEdges expected))))))))

(deftest contains-point-matches-a5
  (doseq [name ["PENTAGON" "TRIANGLE" "synthetic0"]]
    (testing name
      (let [expected (->pentagon name)
            p (pentagon/shape (:vertices expected))]
        ;; Numerically equal: JSON spells A5's 1 as an integer
        (is (every? true? (map == (:containsPoint expected)
                               (mapv #(pentagon/contains-point p %) (:points probe)))))))))

(defn- clip-close? [actual expected]
  (cond
    (nil? actual) (nil? expected)
    (nil? expected) false
    :else (and (close? (:start actual) (:start expected))
               (close? (:end actual) (:end expected))
               (close? (:exit-edge-t actual) (:exitEdgeT expected)))))

(deftest clip-segment-matches-a5
  (doseq [name ["PENTAGON" "TRIANGLE" "synthetic0"]]
    (testing name
      (let [expected (->pentagon name)
            p (pentagon/shape (:vertices expected))]
        (is (= (count (:clipSegment expected)) (count (:segments probe))))
        (doseq [[[a b] e] (map vector (:segments probe) (:clipSegment expected))]
          (is (clip-close? (pentagon/clip-segment p a b) e)
              (str "clip " a " -> " b)))))))

(deftest intersects-segment-matches-a5
  (doseq [name ["PENTAGON" "TRIANGLE" "synthetic0"]]
    (testing name
      (let [expected (->pentagon name)
            p (pentagon/shape (:vertices expected))]
        (is (= (:intersectsSegment expected)
               (mapv (fn [[a b]] (pentagon/intersects-segment? p a b)) (:segments probe))))))))
