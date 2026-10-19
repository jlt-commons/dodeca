(ns dodeca.core.tiling-test
  "Ported from A5's tests/tiling.test.ts."
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.core.tiling :as tiling]
            [dodeca.fixture :as fx]
            [dodeca.geometry.pentagon :as pentagon]
            [dodeca.matchers :refer [close-to?]]))

(def fixtures (fx/load-fixture "tiling.json"))

(defn- matches? [p {:keys [vertices area center]}]
  (is (= (count vertices) (count p)))
  (doseq [[[x y] [ex ey]] (map vector p vertices)]
    (is (close-to? x ex 15))
    (is (close-to? y ey 15)))
  (is (close-to? (pentagon/area p) area 15))
  (let [[cx cy] (pentagon/get-center p)]
    (is (close-to? cx (first center) 15))
    (is (close-to? cy (second center) 15))))

(deftest get-pentagon-vertices
  (doseq [{{:keys [resolution quintant triple flavor] :as input} :input output :output}
          (:getPentagonVertices fixtures)]
    (testing (str input)
      (matches? (tiling/get-pentagon-vertices resolution quintant triple flavor) output))))

(deftest get-pentagon-center
  (doseq [{{:keys [resolution quintant triple flavor] :as input} :input {:keys [center]} :output}
          (:getPentagonVertices fixtures)]
    (let [[cx cy] (tiling/get-pentagon-center resolution quintant triple flavor)]
      (is (close-to? cx (first center) 15) (str input))
      (is (close-to? cy (second center) 15) (str input)))))

(deftest get-quintant-vertices
  (doseq [{{:keys [quintant]} :input output :output} (:getQuintantVertices fixtures)]
    (testing (str quintant)
      (matches? (tiling/get-quintant-vertices quintant) output))))

(deftest get-face-vertices
  (matches? (tiling/get-face-vertices) (:getFaceVertices fixtures)))

(deftest get-quintant-polar
  (doseq [{{:keys [polar]} :input {:keys [quintant]} :output} (:getQuintantPolar fixtures)]
    (is (= quintant (tiling/get-quintant-polar polar)) (str polar))))
