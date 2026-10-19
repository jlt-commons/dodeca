;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.utils.unsigned
  "A5 holds a cell ID in a BigInt of 64 bits; dodeca holds the same bits in a
  long, so ids at or above 2^63 read as negative. Order them with
  `unsigned-compare`, shift them right with `ushr`, spell them with `cell->hex`.

  Jolt promotes a long to a BigInt when arithmetic overflows, so the places A5
  relies on wrapping use the unchecked forms.

  Jolt has `Long/numberOfLeadingZeros` and `Long/bitCount` but not
  `numberOfTrailingZeros`, so `trailing-zeros` isolates the lowest set bit and
  reads its position off the leading zeros."
  (:require [clojure.string :as str]))

(def ^:private min-long -9223372036854775808)

(defn ushr
  "BigInt's `>>` on a 64-bit value: logical, not sign-propagating."
  [x n]
  (unsigned-bit-shift-right x n))

(defn lowest-set-bit
  "x & -x: a tagged cell's tag."
  [x]
  (bit-and x (unchecked-negate x)))

(defn trailing-zeros
  "The number of zero bits below the lowest set bit; 64 for zero."
  [x]
  (if (zero? x)
    64
    (- 63 (Long/numberOfLeadingZeros (lowest-set-bit x)))))

(defn bit-length
  "One past the position of the highest set bit, reading x as unsigned; 64 for
  a negative x, whose top bit is set."
  [x]
  (- 64 (Long/numberOfLeadingZeros x)))

(defn leading-zeros
  [x]
  (Long/numberOfLeadingZeros x))

(defn unsigned-compare
  "compare, reading both as unsigned 64-bit."
  [a b]
  (compare (bit-xor a min-long) (bit-xor b min-long)))

(defn u<
  "<, reading both as unsigned 64-bit: BigInt's < on two cell IDs or slots."
  [a b]
  (< (bit-xor a min-long) (bit-xor b min-long)))

(defn u<=
  "<=, reading both as unsigned 64-bit."
  [a b]
  (<= (bit-xor a min-long) (bit-xor b min-long)))

(defn hex->cell
  "The 64 bits a hex string spells. A5's `u64ToHex` drops leading zeros and its
  fixtures pad to 16; both parse."
  [s]
  (reduce (fn [acc c] (bit-or (bit-shift-left acc 4) (Character/digit c 16)))
          0
          s))

(defn cell->hex
  "A cell ID as 16 lowercase hex digits, the form the fixtures use."
  [x]
  (let [s (Long/toHexString x)]
    (str (apply str (repeat (- 16 (count s)) "0")) s)))
