(ns dodeca.utils.unsigned-spec
  (:require [writ.spec :refer [spec assume ann law graph refine question]]
            [dodeca.utils.unsigned :as u :refer [cell->hex hex->cell lowest-set-bit trailing-zeros]]))

(spec dodeca.utils.unsigned)

(assume Long/numberOfLeadingZeros [Int -> Int])
(assume Long/toHexString [Int -> String])
(assume Character/digit [Char Int -> Int])

(ann ushr [Int Int -> Int])
(ann lowest-set-bit [Int -> Int])
(ann trailing-zeros [Int -> Int])
(ann bit-length [Int -> Int])
(ann leading-zeros [Int -> Int])
(ann unsigned-compare [Int Int -> Int])
(ann u< [Int Int -> Bool])
(ann u<= [Int Int -> Bool])
(ann hex->cell [String -> Int])
(ann cell->hex [Int -> String])

;; A tag is what `getResolution` reads a resolution off: a single bit, or zero
;; for the world cell.
(defn single-bit? [y]
  (or (zero? y) (= y (u/lowest-set-bit y))))

(refine Tag [x Int] (single-bit? x))

;; The three forms an ID takes: the 64 bits, the hex string the fixtures spell
;; them with, and the tag singled out of them.
(graph unsigned
  {:states {:id Int
            :hex String
            :tag Tag}
   :edges {:id {[cell->hex] #{:hex}
                [lowest-set-bit] #{:tag}}
           :hex {[hex->cell] #{:id}}
           :tag {[trailing-zeros] #{:id}}}})

(law hex-length
  (forall [x Int]
    (= 16 (count (u/cell->hex x)))))

(law hex-round-trip
  (forall [x Int]
    (= (u/hex->cell (u/cell->hex x)) x)))

(law hex-canonical
  (forall [x Int]
    (= (u/cell->hex (u/hex->cell (u/cell->hex x)))
       (u/cell->hex x))))

(law lowest-set-bit-is-a-tag
  (forall [x Int]
    (single-bit? (u/lowest-set-bit x))))

(law tag-sits-at-its-trailing-zeros
  (forall [x Int]
    (or (zero? x)
        (= (u/lowest-set-bit x) (bit-shift-left 1 (u/trailing-zeros x))))))

(law trailing-zeros-bounded
  (forall [x Int]
    (<= (u/trailing-zeros x) 64)))

(law zero-has-no-set-bit
  (and (= 0 (u/lowest-set-bit 0))
       (= 64 (u/trailing-zeros 0))
       (= 0 (u/bit-length 0))
       (= 64 (u/leading-zeros 0))))

(law bit-length-meets-leading-zeros
  (forall [x Int]
    (= 64 (+ (u/bit-length x) (u/leading-zeros x)))))

;; Ordering ids needs the unsigned order: a res-30 id with its top bit set is
;; the largest kind of id, not a negative number.
(law unsigned-compare-never-negative-about-itself
  (forall [x Int]
    (and (not (neg? (u/unsigned-compare x 0)))
         (not (pos? (u/unsigned-compare 0 x))))))

(law unsigned-compare-reflexive
  (forall [x Int y Int]
    (and (zero? (u/unsigned-compare x x))
         (= (zero? (u/unsigned-compare x y)) (= x y)))))

(law unsigned-compare-antisymmetric
  (forall [x Int y Int]
    (= (u/unsigned-compare x y) (- (u/unsigned-compare y x)))))

(law u<-agrees-with-unsigned-compare
  (forall [x Int y Int]
    (and (= (u/u< x y) (neg? (u/unsigned-compare x y)))
         (= (u/u<= x y) (not (pos? (u/unsigned-compare x y)))))))

(law u<-top-bit-above-every-non-negative
  (forall [x Int]
    (or (neg? x)
        (u/u< x (u/hex->cell "8000000000000000")))))

(law ushr-of-a-non-negative-is-division
  (forall [x Int n Int]
    (or (neg? x)
        (<= 63 n)
        (= (u/ushr x n) (quot x (bit-shift-left 1 n))))))

(law lowest-set-bit-named
  (and (= 1 (u/lowest-set-bit 1))
       (= 4 (u/lowest-set-bit 12))
       (= (u/hex->cell "8000000000000000")
          (u/lowest-set-bit (u/hex->cell "8000000000000000")))))

(law hex-named
  (and (= "0000000000000000" (u/cell->hex 0))
       (= "ffffffffffffffff" (u/cell->hex -1))
       (= -1 (u/hex->cell "ffffffffffffffff"))))
