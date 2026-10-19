(ns dodeca.core.origin-test
  "Origins are computed rather than taken from a fixture, so the expected values
  come from A5's own module via scripts/probe-origin.mjs."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [dodeca.core.dodecahedron-quaternions :as dq]
            [dodeca.core.origin :as origin])
  (:import [java.io File]))

(def tolerance 1e-12)

(def probe
  (json/read-str (slurp (File. "test/fixtures/probes/origin.json")) :key-fn keyword))

(defn- close? [a b]
  (< (Math/abs (- (double a) (double b))) tolerance))

(defn- vec-close? [a b]
  (and (= (count a) (count b)) (every? true? (map close? a b))))

(defn- orientation-of [o]
  (mapv keyword o))

(deftest quaternions-match-a5
  (is (= (count (:quaternions probe)) (count dq/quaternions)))
  (doseq [[actual expected] (map vector dq/quaternions (:quaternions probe))]
    (is (vec-close? actual expected) (str "quaternion " actual))))

(deftest origins-match-a5
  (is (= (count (:origins probe)) (count origin/origins)))
  (doseq [[actual expected] (map vector origin/origins (:origins probe))]
    (testing (str "origin " (:id expected))
      (is (= (:id expected) (:id actual)))
      (is (vec-close? (:axis expected) (:axis actual)))
      (is (vec-close? (:quat expected) (:quat actual)))
      (is (vec-close? (:inverseQuat expected) (:inverse-quat actual)))
      (is (close? (:angle expected) (:angle actual)))
      (is (= (orientation-of (:orientation expected)) (:orientation actual)))
      (is (= (:firstQuintant expected) (:first-quintant actual))))))

(deftest haversine-matches-a5
  (doseq [{:keys [point out]} (:haversine probe)]
    (is (vec-close? out (mapv #(origin/haversine point (:axis %)) origin/origins))
        (str "haversine from " point))))

(deftest nearest-origin-matches-a5
  (doseq [{:keys [point nearest]} (:nearest probe)]
    (is (= nearest (:id (origin/find-nearest-origin point))) (str "from " point))))

(deftest nearest-origins-matches-a5
  (doseq [{:keys [point ids]} (:nearest3 probe)]
    (is (= ids (mapv :id (origin/find-nearest-origins point 3))) (str "from " point))))

(deftest is-nearest-origin-matches-a5
  (doseq [{:keys [point id out]} (:isNearest probe)]
    (is (= out (origin/is-nearest-origin point (nth origin/origins id)))
        (str "origin " id " from " point))))

(deftest face-step-matches-a5
  (is (= (:faceStep probe) (mapv origin/face-step origin/origins))))

(deftest quintant-to-segment-matches-a5
  (doseq [{o :origin :keys [quintant segment orientation]} (:quintantToSegment probe)]
    (let [actual (origin/quintant->segment quintant (nth origin/origins o))]
      (is (= segment (:segment actual)) (str "origin " o " quintant " quintant))
      (is (= (keyword orientation) (:orientation actual)) (str "origin " o " quintant " quintant)))))

(deftest segment-to-quintant-matches-a5
  (doseq [{o :origin :keys [segment quintant orientation]} (:segmentToQuintant probe)]
    (let [actual (origin/segment->quintant segment (nth origin/origins o))]
      (is (= quintant (:quintant actual)) (str "origin " o " segment " segment))
      (is (= (keyword orientation) (:orientation actual)) (str "origin " o " segment " segment)))))
