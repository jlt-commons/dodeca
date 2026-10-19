(ns dodeca.smoke-test
  (:require [clojure.test :refer [deftest is]]
            [dodeca.fixture :as fx]))

(deftest loads-a-fixture
  (let [f (fx/load-fixture "constants.json")]
    (is (map? f))
    (is (= 1.618033988749895 (get-in f [:φ :value])))))
