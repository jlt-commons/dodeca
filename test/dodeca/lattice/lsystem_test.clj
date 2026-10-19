(ns dodeca.lattice.lsystem-test
  "Ported from A5's tests/lattice/lsystem.test.ts."
  (:require [clojure.test :refer [deftest is]]
            [dodeca.fixture :as fx]
            [dodeca.lattice.curve :refer [round-to-triple]]
            [dodeca.lattice.lsystem.index :as lsystem]))

(def fixtures (fx/load-fixture "lattice/lsystem.json"))

(defn- label [{:keys [s resolution orientation]}]
  (str "s=" s " res=" resolution " ori=" orientation))

(deftest s->cell
  (doseq [{:keys [s resolution orientation x y z flavor] :as f} (:sToCell fixtures)]
    (is (= {:triple {:x x :y y :z z} :flavor flavor} (lsystem/s->cell s resolution (keyword orientation)))
        (label f))
    (is (= {:x x :y y :z z} (lsystem/s->triple s resolution (keyword orientation))) (label f))
    (is (= s (lsystem/triple->s-lattice {:x x :y y :z z} resolution (keyword orientation))) (label f))))

(deftest point->s
  (doseq [{:keys [i j resolution orientation s]} (:pointToS fixtures)]
    (is (= s (lsystem/triple->s-lattice (round-to-triple [i j] resolution) resolution (keyword orientation)))
        (str "(" i "," j ") res=" resolution " ori=" orientation))))

(def ^:private orientations [:uv :vu :uw :wu :vw :wv])

(defn- root [orientation]
  (:node (lsystem/triple->curve-node {:x 0 :y 0 :z 0} 0 orientation)))

(defn- step
  "Step to the child with `digit`, checking it against s->cell, and the
  descent state against triple->curve-node's from the child's triple."
  [node s digit resolution orientation]
  (let [{:keys [triple flavor] below :node} (lsystem/curve-child node digit resolution orientation)
        child-s (+ (* s 4) digit)]
    (is (= {:triple triple :flavor flavor} (lsystem/s->cell child-s resolution orientation))
        (str orientation " s=" child-s " res=" resolution))
    (is (= {:s child-s :flavor flavor :node below} (lsystem/triple->curve-node triple resolution orientation))
        (str orientation " s=" child-s " res=" resolution))
    {:node below :s child-s}))

(deftest curve-child-and-triple->curve-node
  (doseq [orientation orientations]
    ;; Every cell through level 3, then one deep path to level 29 (A5 resolution 30)
    (loop [stack [[(root orientation) 0 0]]]
      (when-let [[node s resolution] (peek stack)]
        (let [stack (pop stack)]
          (if (= resolution 3)
            (recur stack)
            (recur (into stack (map (fn [digit]
                                      (let [child (step node s digit (inc resolution) orientation)]
                                        [(:node child) (:s child) (inc resolution)]))
                                    (range 4))))))))
    (reduce (fn [child resolution]
              (step (:node child) (:s child) (mod (+ (* resolution 7) (int (first (name orientation)))) 4)
                    resolution orientation))
            {:node (root orientation) :s 0}
            (range 1 30))))
