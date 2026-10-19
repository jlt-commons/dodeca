(ns dodeca.projections.gnomonic-test
  "Ported from A5's tests/projections/gnomonic.test.ts."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.fixture :as fx]
            [dodeca.matchers :refer [close-to-array?]]
            [dodeca.projections.gnomonic :as gnomonic]))

(def test-data (fx/load-fixture "projections/gnomonic.json"))

(deftest forward
  (doseq [{:keys [input expected]} (:forward test-data)]
    (is (close-to-array? (gnomonic/forward input) expected) (str input))
    (is (close-to-array? (gnomonic/inverse (gnomonic/forward input)) input) (str input))))

(deftest inverse
  (doseq [{:keys [input expected]} (:inverse test-data)]
    (is (close-to-array? (gnomonic/inverse input) expected) (str input))
    (is (close-to-array? (gnomonic/forward (gnomonic/inverse input)) input) (str input))))
