(ns dodeca.projections.authalic-test
  "Ported from A5's tests/projections/authalic.test.ts."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.fixture :as fx]
            [dodeca.matchers :refer [close-to?]]
            [dodeca.projections.authalic :as authalic]))

(def test-data (fx/load-fixture "projections/authalic.json"))

(deftest forward
  (doseq [{:keys [input expected]} (:forward test-data)]
    (is (close-to? (authalic/forward input) expected 10) (str input))
    (is (close-to? (authalic/inverse (authalic/forward input)) input 15) (str input))))

(deftest inverse
  (doseq [{:keys [input expected]} (:inverse test-data)]
    (is (close-to? (authalic/inverse input) expected 10) (str input))
    (is (close-to? (authalic/forward (authalic/inverse input)) input 15) (str input))))
