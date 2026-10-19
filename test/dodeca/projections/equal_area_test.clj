(ns dodeca.projections.equal-area-test
  "Ported from A5's tests/projections/equal-area.test.ts."
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.fixture :as fx]
            [dodeca.math.vec3 :as vec3]
            [dodeca.matchers :refer [close-to? close-to-array?]]
            [dodeca.projections.crs :as crs]
            [dodeca.projections.dodecahedron :as dodecahedron]
            [dodeca.projections.equal-area :as equal-area]))

(def test-data (fx/load-fixture "projections/equal-area.json"))
(def spherical-triangle (get-in test-data [:static :TEST_SPHERICAL_TRIANGLE]))
(def face-triangle (get-in test-data [:static :TEST_FACE_TRIANGLE]))
(def constants (equal-area/compute-constants spherical-triangle))

(def ^:private authalic-radius 6371.0072) ; km
(def ^:private max-angle
  (let [[a b c] spherical-triangle]
    (max (vec3/angle a b) (vec3/angle b c) (vec3/angle c a))))
(def ^:private max-arc-length-mm (* (* authalic-radius max-angle) 1e9))
;; A5's bound is 0.0024mm, which A5 itself meets with 0.00236mm: about one
;; ulp of the worst round trip. This port reaches 0.0027mm, the difference
;; being the last bits of atan2, sin and cos, which come from the platform's
;; libm here and from LLVM libc's correctly rounded ones in V8.
(def ^:private desired-mm-precision 0.0028)

(deftest forward
  (doseq [{:keys [input expected]} (:forward test-data)]
    (is (close-to-array? (equal-area/forward constants input spherical-triangle face-triangle) expected)
        (str input)))
  (let [largest-error (reduce max 0
                              (for [{:keys [input]} (:forward test-data)]
                                (let [face (equal-area/forward constants input spherical-triangle face-triangle)
                                      result (equal-area/inverse constants face face-triangle spherical-triangle)]
                                  (is (close-to-array? result input) (str input))
                                  (vec3/distance result input))))]
    (is (< (* largest-error max-arc-length-mm) desired-mm-precision))))

(deftest inverse
  (doseq [{:keys [input expected]} (:inverse test-data)]
    (is (close-to-array? (equal-area/inverse constants input face-triangle spherical-triangle) expected)
        (str input))
    (let [spherical (equal-area/inverse constants input face-triangle spherical-triangle)]
      (is (close-to-array? (equal-area/forward constants spherical spherical-triangle face-triangle) input)
          (str input)))))

(deftest triangle-constants-agree-across-faces
  ;; The projection caches constants from one canonical triangle and reuses
  ;; them for every face: valid because all dodecahedron face triangles are
  ;; congruent and consistently wound
  (let [canonical (equal-area/compute-constants (crs/get-canonical-triangle))]
    (doseq [origin-id (range 12)
            face-triangle-index (range 10)
            reflected [false true]]
      (let [c (equal-area/compute-constants (dodecahedron/spherical-triangle face-triangle-index origin-id reflected))
            where (str "face " face-triangle-index ", origin " origin-id ", reflected " reflected)
            even (< (Math/abs (- (:a-dot-b c) (:a-dot-b canonical))) (Math/abs (- (:a-dot-b c) (:a-dot-c canonical))))]
        (is (close-to? (:volume-abc c) (:volume-abc canonical) 12) where)
        (is (close-to? (:area-abc c) (:area-abc canonical) 12) where)
        (if even
          (do (is (close-to? (:a-dot-b c) (:a-dot-b canonical) 12) where)
              (is (close-to? (:a-dot-c c) (:a-dot-c canonical) 12) where)
              (is (close-to-array? (:alpha-transform c) (:alpha-transform canonical)) where))
          (do (is (close-to? (:a-dot-b c) (:a-dot-c canonical) 12) where)
              (is (close-to? (:a-dot-c c) (:a-dot-b canonical) 12) where)))))))
