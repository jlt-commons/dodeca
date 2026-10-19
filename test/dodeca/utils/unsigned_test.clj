(ns dodeca.utils.unsigned-test
  (:require [clojure.test :refer [deftest is testing]]
            [dodeca.fixture :as fx]
            [dodeca.utils.unsigned :as u]))

(def test-id-hex
  (concat (fx/load-fixture "test-ids.json")
          (:testIds (fx/load-fixture "serialization.json"))
          (map :hex (:res30Locations (fx/load-fixture "serialization.json")))))

(deftest hex-round-trip
  (doseq [h test-id-hex]
    (is (= (u/cell->hex (u/hex->cell h)) (u/cell->hex (u/hex->cell h))))
    (is (= (u/hex->cell (u/cell->hex (u/hex->cell h)))
           (u/hex->cell h))
        h)))

(deftest hex-edge-cases
  (is (= 0 (u/hex->cell "0")))
  (is (= 0 (u/hex->cell "0000000000000000")))
  (is (= 1 (u/hex->cell "1")))
  (is (= -1 (u/hex->cell "ffffffffffffffff")))
  (is (= "0000000000000000" (u/cell->hex 0)))
  (is (= "ffffffffffffffff" (u/cell->hex -1)))
  (is (= "8000000000000000" (u/cell->hex (u/hex->cell "8000000000000000")))))

(deftest top-bit-is-the-largest-unsigned-value
  (is (= 1 (u/unsigned-compare (u/hex->cell "8000000000000000") 0)))
  (is (= -1 (u/unsigned-compare 0 (u/hex->cell "8000000000000000"))))
  (is (= 1 (u/unsigned-compare -1 (u/hex->cell "8000000000000000"))))
  (is (= 0 (u/unsigned-compare -1 -1))))

(defn- bin->cell [s]
  (reduce (fn [acc c] (bit-or (bit-shift-left acc 1) (if (= c \1) 1 0))) 0 s))

(deftest resolution-masks-set-the-tag-bit
  (let [masks (:resolutionMasks (fx/load-fixture "serialization.json"))]
    (is (= 31 (count masks)))
    (doseq [[r mask] (map-indexed vector masks)]
      (let [bits (bin->cell mask)]
        (testing (str "resolution " r)
          (is (= 1 (Long/bitCount bits)))
          (is (= (if (= r 0) 57 (if (= r 1) 56 (max (- 59 (* 2 r)) 0)))
                 (u/trailing-zeros bits))))))))

(deftest trailing-zeros-edge-cases
  (is (= 64 (u/trailing-zeros 0)))
  (is (= 0 (u/trailing-zeros 1)))
  (is (= 63 (u/trailing-zeros (u/hex->cell "8000000000000000"))))
  (is (= 1 (u/trailing-zeros 2)))
  (is (= 4 (u/trailing-zeros 16))))

(deftest bit-length-and-leading-zeros
  (is (= 0 (u/bit-length 0)))
  (is (= 1 (u/bit-length 1)))
  (is (= 8 (u/bit-length 255)))
  (is (= 64 (u/bit-length -1)))
  (is (= 64 (u/bit-length (u/hex->cell "8000000000000000"))))
  (is (= 63 (u/leading-zeros 1)))
  (is (= 0 (u/leading-zeros -1)))
  (is (= 64 (u/leading-zeros 0))))

(deftest lowest-set-bit
  (is (= 0 (u/lowest-set-bit 0)))
  (is (= 1 (u/lowest-set-bit 1)))
  (is (= 4 (u/lowest-set-bit 12)))
  (is (= (u/hex->cell "8000000000000000")
         (u/lowest-set-bit (u/hex->cell "8000000000000000")))))
