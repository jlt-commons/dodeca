(ns dodeca.core.serialization-test
  "Ported from A5's tests/serialization.test.ts and tests/hex.test.ts."
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.core.hex :as hex]
            [dodeca.core.origin :as origin]
            [dodeca.core.serialization :as s]
            [dodeca.fixture :as fx]
            [dodeca.utils.unsigned :as u]))

(def fixtures (fx/load-fixture "serialization.json"))
(def resolution-masks (:resolutionMasks fixtures))
(def test-ids (:testIds fixtures))

(def origin0 (nth origin/origins 0))

(defn- binary->long [b]
  (reduce (fn [acc c] (bit-or (bit-shift-left acc 1) (if (= c \1) 1 0))) 0 b))

(defn- long->binary [x]
  (apply str (for [i (range 63 -1 -1)] (if (bit-test x i) \1 \0))))

(defn- thrown-message [f]
  (try (f) nil (catch Exception e (ex-message e))))

(defn- ascending? [cells]
  (every? neg? (map u/unsigned-compare cells (rest cells))))

(deftest serialize-masks
  (is (= (inc s/max-resolution) (count resolution-masks)))
  (testing "encodes resolution correctly for different values"
    ;; Origin 0 has first quintant 4, so segment 4 is the start of the curve
    (doseq [[i mask] (map-indexed vector resolution-masks)]
      (is (= mask (long->binary (s/serialize {:origin origin0 :segment 4 :s 0 :resolution i})))
          (str "resolution " i))))
  (testing "correctly extracts resolution"
    (doseq [[i mask] (map-indexed vector resolution-masks)]
      (is (= 64 (count mask)))
      (is (= i (s/get-resolution (binary->long mask)))))))

(deftest serialize-origin-segment-s
  (is (= 2 (s/serialize {:origin origin0 :segment 4 :s 0 :resolution (dec s/max-resolution)}))))

(deftest serialize-errors
  (is (= "S (16) is too large for resolution level 3"
         (thrown-message #(s/serialize {:origin origin0 :segment 0 :s 16 :resolution 3}))))
  (is (= "Resolution (31) is too large"
         (thrown-message #(s/serialize {:origin origin0 :segment 0 :s 0 :resolution 31})))))

(deftest round-trip
  (doseq [n (range 1 12)
          :let [prefix (let [b (Long/toBinaryString (* 5 n))]
                         (str (apply str (repeat (- 6 (count b)) \0)) b))]
          mask (subvec resolution-masks s/first-curve-resolution s/max-resolution)]
    (let [serialized (binary->long (str prefix (subs mask 6)))]
      (is (= serialized (s/serialize (s/deserialize serialized))) (str "origin " n " " mask))))
  (testing "test ids"
    (doseq [id test-ids]
      (let [serialized (hex/hex->u64 id)]
        (is (= serialized (s/serialize (s/deserialize serialized))) id)))))

(deftest hierarchy-round-trip
  (doseq [id test-ids
          :let [cell (hex/hex->u64 id)
                resolution (s/get-resolution cell)]
          :when (< resolution s/max-resolution)
          :let [child (first (s/cell->children cell))]
          :when (= (s/get-resolution child) (inc resolution))]
    (is (= cell (s/cell->parent child)) id)
    (is (every? #(= cell %) (map s/cell->parent (s/cell->children cell))) id)))

(deftest same-resolution-is-identity
  (doseq [id test-ids
          :let [cell (hex/hex->u64 id)
                resolution (s/get-resolution cell)]]
    (is (= [cell] (s/cell->children cell resolution)) id)
    (is (= cell (s/cell->parent cell resolution)) id)))

(deftest cell-to-parent-finer-target
  (doseq [{:keys [cell resolution]} (:finerParentTargets fixtures)]
    (let [msg (thrown-message #(s/cell->parent (hex/hex->u64 cell) resolution))]
      (is (and msg (re-find #"must be equal to or less than current resolution" msg))
          (str cell " -> " resolution)))))

(deftest children-ascending
  (doseq [res [0 1 2]]
    (is (ascending? (s/cell->children s/world-cell res))))
  (doseq [cell (s/get-res0-cells)
          res [1 2 3]]
    (is (ascending? (s/cell->children cell res)))))

(deftest non-curve-hierarchies
  (let [cell (s/serialize {:origin origin0 :segment 0 :s 0 :resolution 0})
        children (s/cell->children cell)]
    (is (= 5 (count children)))
    (is (every? #(= cell (s/cell->parent %)) children)))
  (let [cell (s/serialize {:origin origin0 :segment 0 :s 0 :resolution 1})
        children (s/cell->children cell)]
    (is (= 4 (count children)))
    (is (every? #(= cell (s/cell->parent %)) children)))
  (let [cell (s/serialize {:origin origin0 :segment 0 :s 0 :resolution 2})
        children (s/cell->children (s/cell->parent cell 1))]
    (is (= 4 (count children)))
    (is (some #{cell} children))))

(deftest low-resolution-chain
  (let [cells (mapv #(s/serialize {:origin origin0 :segment 0 :s 0 :resolution %}) [0 1 2 3 4])]
    (doseq [i (range 1 (count cells))]
      (is (= (nth cells (dec i)) (s/cell->parent (nth cells i)))))
    (doseq [i (range (dec (count cells)))]
      (is (some #{(nth cells (inc i))} (s/cell->children (nth cells i)))))))

(deftest base-cell-division-counts
  (loop [cells [(s/serialize {:origin origin0 :segment 0 :s 0 :resolution -1})]
         [expected & more] [12 60 240 960]]
    (when expected
      (let [children (vec (mapcat s/cell->children cells))]
        (is (= expected (count children)))
        (recur children more)))))

(deftest is-child-of
  (doseq [res (range s/first-curve-resolution s/max-resolution)]
    (let [cell (s/serialize {:origin origin0 :segment 0 :s 0 :resolution res})]
      (is (s/child-of? cell cell res))))
  (doseq [res (range s/first-curve-resolution (dec s/max-resolution))]
    (let [parent (s/serialize {:origin origin0 :segment 0 :s 0 :resolution res})]
      (doseq [child (s/cell->children parent)]
        (is (s/child-of? child parent res)))))
  (let [parent (s/serialize {:origin origin0 :segment 0 :s 0 :resolution 5})
        descendants (reduce (fn [cells _] (mapcat s/cell->children cells)) [parent] (range 6 10))]
    (is (every? #(s/child-of? % parent 5) descendants)))
  (let [children (s/cell->children (s/serialize {:origin origin0 :segment 0 :s 0 :resolution 4}))]
    (doseq [i (range (count children)) j (range (count children)) :when (not= i j)]
      (is (not (s/child-of? (nth children i) (nth children j) 5)))))
  (let [grandparent (s/serialize {:origin origin0 :segment 0 :s 0 :resolution 4})
        parent (first (s/cell->children grandparent))]
    (is (not (s/child-of? grandparent parent 5)))))

(deftest res0-cells
  (let [cells (s/get-res0-cells)
        expected (map-indexed (fn [i h] (str h (apply str (repeat (- (if (< i 4) 15 16) (count h)) \0))))
                              ["2" "6" "a" "e" "12" "16" "1a" "1e" "22" "26" "2a" "2e"])]
    (is (= 12 (count cells)))
    (is (every? zero? (map s/get-resolution cells)))
    (is (= expected (map hex/u64->hex cells)))))

(deftest resolution-30
  (testing "getResolution detects res 30 from LSB"
    (is (= 30 (s/get-resolution 1)))
    (is (= 30 (s/get-resolution 3)))
    (is (= 30 (s/get-resolution -1))))
  (testing "round trip for valid quintants (0-41)"
    (doseq [q (range 42)]
      (let [origin-id (quot q 5)
            o (nth origin/origins origin-id)
            segment (mod (+ (mod q 5) (:first-quintant o)) 5)
            serialized (s/serialize {:origin o :segment segment :s 0 :resolution 30})
            deserialized (s/deserialize serialized)]
        (is (= 30 (s/get-resolution serialized)))
        (cond
          (<= q 31) (is (= 1 (bit-and serialized 1)))
          (<= q 39) (is (= 2r100 (bit-and serialized 2r111)))
          :else (is (= 2r10000 (bit-and serialized 2r11111))))
        (is (= origin-id (:id (:origin deserialized))))
        (is (= segment (:segment deserialized)))
        (is (= 0 (:s deserialized)))
        (is (= 30 (:resolution deserialized)))
        (is (= serialized (s/serialize deserialized))))))
  (testing "round trip with non-zero S in each encoding"
    (doseq [[origin-id tag mask] [[0 1 1] [7 2r100 2r111] [8 2r10000 2r11111]]
            S [0 1 42 (dec (bit-shift-left 1 58))]]
      (let [o (nth origin/origins origin-id)
            serialized (s/serialize {:origin o :segment (:first-quintant o) :s S :resolution 30})
            deserialized (s/deserialize serialized)]
        (is (= tag (bit-and serialized mask)))
        (is (= S (:s deserialized)))
        (is (= 30 (:resolution deserialized)))
        (is (= serialized (s/serialize deserialized))))))
  (testing "bit layouts"
    (let [o (nth origin/origins 0)]
      (is (= 1 (s/serialize {:origin o :segment (:first-quintant o) :s 0 :resolution 30})))
      (is (= 2r11 (s/serialize {:origin o :segment (:first-quintant o) :s 1 :resolution 30}))))
    (let [o (nth origin/origins 8)]
      (is (= 2r10000 (s/serialize {:origin o :segment (:first-quintant o) :s 0 :resolution 30})))
      (is (= 2r110000 (s/serialize {:origin o :segment (:first-quintant o) :s 1 :resolution 30}))))
    (let [o (nth origin/origins 6)
          segment (mod (+ 2 (:first-quintant o)) 5)]
      (is (= 2r100 (s/serialize {:origin o :segment segment :s 0 :resolution 30})))
      (is (= 2r1100 (s/serialize {:origin o :segment segment :s 1 :resolution 30})))))
  (testing "falls back to res 29 for quintant > 41"
    (let [o (nth origin/origins 9)
          segment (:first-quintant o)]
      (is (= 29 (s/get-resolution (s/serialize {:origin o :segment segment :s 0 :resolution 30}))))
      (let [cell (s/serialize {:origin o :segment segment :s 7 :resolution 30})]
        (is (= 29 (s/get-resolution cell)))
        (is (= 1 (:s (s/deserialize cell))))))
    (let [o (nth origin/origins 11)
          cell (s/serialize {:origin o :segment (:first-quintant o) :s 100 :resolution 30})]
      (is (= 29 (s/get-resolution cell)))
      (is (= 25 (:s (s/deserialize cell))))
      (is (= 11 (:id (:origin (s/deserialize cell)))))))
  (testing "throws for S too large"
    (let [o (nth origin/origins 0)
          msg (thrown-message #(s/serialize {:origin o :segment (:first-quintant o)
                                             :s (bit-shift-left 1 58) :resolution 30}))]
      (is (and msg (re-find #"too large for resolution level 30" msg)))))
  (testing "parents and children between res 29 and 30"
    (let [o (nth origin/origins 0)
          segment (:first-quintant o)]
      (doseq [i (range 4)]
        (let [parent (s/cell->parent (s/serialize {:origin o :segment segment :s i :resolution 30}))]
          (is (= 29 (s/get-resolution parent)))
          (is (= 0 (:s (s/deserialize parent))))))
      (let [children (s/cell->children (s/serialize {:origin o :segment segment :s 0 :resolution 29}) 30)]
        (is (= 4 (count children)))
        (doseq [[i child] (map-indexed vector children)]
          (is (= 30 (s/get-resolution child)))
          (is (= i (:s (s/deserialize child))))))
      (let [parent (s/serialize {:origin o :segment segment :s 42 :resolution 29})
            children (s/cell->children parent 30)]
        (is (= 4 (count children)))
        (is (every? #(= parent (s/cell->parent %)) children))))
    (doseq [[origin-id tag mask] [[8 2r10000 2r11111] [7 2r100 2r111]]]
      (let [o (nth origin/origins origin-id)
            parent (s/serialize {:origin o :segment (:first-quintant o) :s 10 :resolution 29})
            children (s/cell->children parent 30)]
        (is (= 4 (count children)))
        (doseq [child children]
          (is (= 30 (s/get-resolution child)))
          (is (= tag (bit-and child mask)))
          (is (= parent (s/cell->parent child)))))))
  (testing "children of res 30 throw"
    (let [o (nth origin/origins 0)
          cell (s/serialize {:origin o :segment (:first-quintant o) :s 0 :resolution 30})
          msg (thrown-message #(s/cell->children cell))]
      (is (and msg (re-find #"exceeds maximum resolution" msg))))))

(deftest resolution-30-locations
  (let [locations (:res30Locations fixtures)]
    (doseq [{h :hex} locations]
      (let [cell (hex/hex->u64 h)]
        (is (= cell (s/serialize (s/deserialize cell))) h)))
    (let [out-of-bounds (filter #(= 29 (:resolution %)) locations)
          in-bounds (filter #(= 30 (:resolution %)) locations)]
      (is (seq out-of-bounds))
      (is (seq in-bounds))
      (doseq [{h :hex} out-of-bounds] (is (= 29 (s/get-resolution (hex/hex->u64 h))) h))
      (doseq [{h :hex} in-bounds] (is (= 30 (s/get-resolution (hex/hex->u64 h))) h)))))

(deftest valid-cells
  (is (s/valid-cell? s/world-cell))
  (doseq [id test-ids]
    (is (s/valid-cell? (hex/hex->u64 id)) id))
  (doseq [{h :hex} (:res30Locations fixtures)]
    (is (s/valid-cell? (hex/hex->u64 h)) h))
  (testing "a tag that is no resolution's, and origins and quintants that don't exist"
    (is (not (s/valid-cell? (bit-shift-left 1 58))))
    (is (not (s/valid-cell? (bit-shift-left 1 6))))
    (is (not (s/valid-cell? (bit-or (bit-shift-left 12 58) (bit-shift-left 1 57)))))
    (is (not (s/valid-cell? (bit-or (bit-shift-left 60 58) (bit-shift-left 1 56)))))
    (is (not (s/valid-cell? (bit-or (bit-shift-left 60 58) (bit-shift-left 1 55)))))))

(deftest hex
  (doseq [[h n] [["1a2b3c" 1715004] ["0" 0] ["ff" 255] ["ffffffff" 4294967295]]]
    (is (= n (hex/hex->u64 h)))
    (is (= h (hex/u64->hex n)))
    (is (= h (hex/u64->hex (hex/hex->u64 h)))))
  (testing "ids with the top bit set"
    (is (= "ffffffffffffffff" (hex/u64->hex -1)))
    (is (= -1 (hex/hex->u64 "ffffffffffffffff")))))
