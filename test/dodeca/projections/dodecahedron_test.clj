(ns dodeca.projections.dodecahedron-test
  "Ported from A5's tests/projections/dodecahedron.test.ts, plus A5's own
  results on every face for seeded points (scripts/probe-projections.mjs)."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.fixture :as fx]
            [dodeca.matchers :refer [close-to-array?]]
            [dodeca.projections.dodecahedron :as dodecahedron]))

(def test-data (fx/load-fixture "projections/dodecahedron.json"))
(def origin-id (get-in test-data [:static :ORIGIN_ID]))
(def probe (fx/load-fixture "probes/projections.json"))

(deftest forward
  (doseq [{:keys [input expected]} (:forward test-data)]
    (is (close-to-array? (dodecahedron/forward input origin-id) expected) (str input))
    (is (close-to-array? (dodecahedron/inverse (dodecahedron/forward input origin-id) origin-id) input)
        (str input))))

(deftest inverse
  (doseq [{:keys [input expected]} (:inverse test-data)]
    (is (close-to-array? (dodecahedron/inverse input origin-id) expected) (str input))
    (is (close-to-array? (dodecahedron/forward (dodecahedron/inverse input origin-id) origin-id) input)
        (str input))))

(defn- same-or-nan?
  "JSON spells NaN as null: the antipode of a face center projects to NaN."
  [actual expected]
  (if (some nil? expected)
    (every? #(Double/isNaN %) actual)
    (close-to-array? actual expected 13)))

(deftest matches-a5-on-every-face
  (doseq [{:keys [lonLat spherical forward]} (:lonLat probe)
          id (range 12)]
    (is (same-or-nan? (dodecahedron/forward spherical id) (nth forward id)) (str lonLat " origin " id)))
  (doseq [{:keys [face inverse]} (:faces probe)
          id (range 12)]
    (is (same-or-nan? (dodecahedron/inverse face id) (nth inverse id)) (str face " origin " id))))
