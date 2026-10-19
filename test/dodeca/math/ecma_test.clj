(ns dodeca.math.ecma-test
  "Expected values are what node gives for the same expressions."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.math.ecma :as ecma]))

(deftest round
  (doseq [[x expected] [[-2.5 -2.0] [2.5 3.0] [0.49999999999999994 0.0] [-1.5 -1.0]
                        [4503599627370497.0 4503599627370497.0] [1.5 2.0] [-0.4 0.0] [7.0 7.0]]]
    (is (= expected (ecma/round x)) (str x))))

(deftest rem
  (doseq [[x y expected] [[7.5 1 0.5] [-7.25 1 -0.25] [1e17 360 280.0]
                          [123456789.123 360 189.12299999594688] [-190.5 360 -190.5]
                          [719.9999999999999 360 359.9999999999999] [540 360 180.0] [0.0 360 0.0]]]
    (is (= expected (ecma/rem x y)) (str x " % " y))))
