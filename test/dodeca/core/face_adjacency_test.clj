(ns dodeca.core.face-adjacency-test
  "Face adjacency, seam maps and the face walk, checked against A5's own output
  via scripts/probe-pentagon.mjs."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [dodeca.core.face-adjacency :as fa])
  (:import [java.io File]))

(def tolerance 1e-12)

(def probe
  (json/read-str (slurp (File. "test/fixtures/probes/pentagon.json")) :key-fn keyword))

(defn- close? [a b]
  (< (Math/abs (- (double a) (double b))) tolerance))

(defn- vec-close? [a b]
  (and (= (count a) (count b)) (every? true? (map close? a b))))

(deftest adjacency-table-matches-a5
  (is (= (count (:faceAdjacency probe)) (count fa/face-adjacency)))
  (doseq [[actual expected] (map vector fa/face-adjacency (:faceAdjacency probe))]
    (is (= (count expected) (count actual)))
    (is (= (mapv vec expected) actual))))

(deftest seam-transform-matches-a5
  (doseq [[origin-id row] (map-indexed vector (:seamTransform probe))
          [quintant expected] (map-indexed vector row)]
    (is (vec-close? expected (fa/seam-transform origin-id quintant))
        (str "face " origin-id " quintant " quintant))))

(deftest seam-triple-matches-a5
  (doseq [{:keys [t maxRow out]} (:seamTriple probe)]
    (let [actual (fa/seam-triple t maxRow)]
      (is (= [(:x out) (:y out) (:z out)]
             [(:x actual) (:y actual) (:z actual)])
          (str "triple " t " max-row " maxRow)))))

(defn- expand-fn [expand]
  (case expand
    "all" (constantly true)
    "none" (constantly false)
    "even" even?))

(deftest walk-faces-matches-a5
  (doseq [{:keys [seeds expand maxRings out]} (:walkFaces probe)]
    (let [f (expand-fn expand)
          actual (if (nil? maxRings) (fa/walk-faces seeds f) (fa/walk-faces seeds f maxRings))]
      (is (= out actual) (str "seeds " seeds " expand " expand " rings " maxRings)))))
