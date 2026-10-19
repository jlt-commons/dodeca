(ns dodeca.utils.vector-test
  "Ported from A5's tests/utils/vector.test.ts, plus the triple product."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.math.vec3 :as vec3]
            [dodeca.matchers :refer [close-to-array?]]
            [dodeca.utils.vector :as v]))

(def A [1 0 0])
(def B [0 1 0])

(deftest slerp
  (is (close-to-array? (v/slerp A B 0.5) [(/ 1 (Math/sqrt 2)) (/ 1 (Math/sqrt 2)) 0] 6))
  (is (close-to-array? (v/slerp A B 0) [1 0 0] 6))
  (is (close-to-array? (v/slerp A B 1) [0 1 0] 6))
  (is (close-to-array? (v/slerp A A 0.5) [1 0 0] 6))
  (let [[x1 y1] (v/slerp A B 0.25)
        [x2 y2] (v/slerp A B 0.75)]
    (is (> x1 y1))
    (is (> y2 x2))))

(deftest slerp-with-context
  (let [ctx (v/precompute-slerp A B)]
    (doseq [t [0 0.25 0.5 0.75 1]]
      (is (= (v/slerp A B t) (v/slerp A B t ctx))))))

(deftest triple-product
  (doseq [[a b c] [[[1 2 3] [4 5 6] [7 8 10]]
                   [[0.3 -0.2 0.9] [0.1 0.7 -0.4] [-0.6 0.5 0.2]]]]
    (is (= (vec3/dot a (vec3/cross b c)) (v/triple-product a b c)))))
