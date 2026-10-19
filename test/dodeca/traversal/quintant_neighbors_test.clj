(ns dodeca.traversal.quintant-neighbors-test
  "Ported from A5's tests/traversal/quintant-neighbors.test.ts."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.fixture :as fx]
            [dodeca.traversal.neighbors :refer [neighbor-deltas]]
            [dodeca.traversal.quintant-neighbors :refer [get-cell-neighbors]]))

(def fixtures (fx/load-fixture "traversal/quintant-neighbors.json"))

(deftest get-cell-neighbors-matches-a5
  (doseq [{{:keys [s resolution orientation]} :input {:keys [neighbors]} :output} fixtures]
    (is (= neighbors (get-cell-neighbors s resolution (keyword orientation)))
        (str "s=" s " res=" resolution " ori=" orientation))))

(deftest neighbor-deltas-all-is-edge-then-vertex
  (doseq [{:keys [edge vertex all]} neighbor-deltas]
    (is (= all (into edge vertex)))))
