(ns dodeca.math-test
  "The math layer against values A5's own modules returned, recorded in
  test/fixtures/probes/math.json by scripts/probe-math.mjs."
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.fixture :as fx]
            [dodeca.math.hypot :as h]
            [dodeca.math.mat2 :as mat2]
            [dodeca.math.quat :as quat]
            [dodeca.math.vec2 :as v2]
            [dodeca.math.vec3 :as v3]))

(def tolerance 1e-10)

(defn- close?
  "Every component within tolerance; numbers are compared directly."
  [expected actual]
  (if (number? expected)
    (<= (Math/abs (- (double expected) (double actual))) tolerance)
    (and (= (count expected) (count actual))
         (every? true? (map close? expected actual)))))

(def op-fn
  {"vec2/add" v2/add
   "vec2/scale" v2/scale
   "vec2/scale-and-add" v2/scale-and-add
   "vec2/negate" v2/negate
   "vec2/lerp" v2/lerp
   "vec2/rotate" v2/rotate
   "vec2/transform-mat2" v2/transform-mat2
   "vec2/transform-mat2d" v2/transform-mat2d
   "vec2/length" v2/length
   "vec3/add" v3/add
   "vec3/subtract" v3/subtract
   "vec3/scale" v3/scale
   "vec3/scale-and-add" v3/scale-and-add
   "vec3/normalize" v3/normalize
   "vec3/cross" v3/cross
   "vec3/lerp" v3/lerp
   "vec3/length" v3/length
   "vec3/distance" v3/distance
   "vec3/dot" v3/dot
   "vec3/angle" v3/angle
   "vec3/transform-quat" v3/transform-quat
   "mat2/from-rotation" mat2/from-rotation
   "quat/conjugate" quat/conjugate})

(deftest matches-a5
  (let [probes (fx/load-fixture "probes/math.json")]
    (is (= 101 (count probes)))
    (doseq [{:keys [op args out]} probes]
      (testing (str op " " (pr-str args))
        (is (close? out (apply (op-fn op) args)))))))

(deftest hypot-is-the-scaled-root-sum-of-squares
  (is (= 5.0 (h/hypot 3 4)))
  (is (= 5.0 (h/hypot 3 4 0)))
  (is (= 0.0 (h/hypot 0 0 0)))
  (is (= ##Inf (h/hypot ##Inf 1)))
  (is (Double/isNaN (h/hypot ##NaN 1)))
  (is (close? 7.0710678118654755 (h/hypot 3 4 5))))

(deftest degenerate-vectors
  (is (close? [0 0 0] (v3/normalize [0 0 0])))
  (is (close? [0 0] (v2/scale [0 0] 5)))
  (is (close? [0 0 0] (v3/cross [0 0 0] [1 2 3]))))
