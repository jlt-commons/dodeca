(ns dodeca.hierarchy-test
  "Ported from A5's tests/hierarchy.test.ts: every cell's pentagon shares
  area with its parent's."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.core.tiling :refer [get-pentagon-vertices]]
            [dodeca.geometry.pentagon :as pentagon]
            [dodeca.lattice.index :refer [s->cell]]))

(defn- generate-cells [resolution orientation]
  (mapv (fn [s]
          (let [{:keys [triple flavor]} (s->cell s resolution orientation)]
            (get-pentagon-vertices resolution 0 triple flavor)))
        (range (bit-shift-left 1 (* 2 resolution)))))

(defn- uncontained-children [resolution orientation]
  (let [parents (generate-cells resolution orientation)
        children (generate-cells (inc resolution) orientation)]
    (keep-indexed (fn [i child]
                    (let [parent (nth parents (quot i 4))]
                      (when-not (some #(pos? (pentagon/contains-point parent %)) child)
                        i)))
                  children)))

(deftest hierarchy
  (doseq [orientation [:uv :vu :uw :wu :vw :wv]
          resolution [1 2 3 4 5 6]]
    (is (empty? (uncontained-children resolution orientation)) (str orientation " " resolution))))
