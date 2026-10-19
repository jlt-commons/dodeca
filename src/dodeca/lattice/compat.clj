;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.lattice.compat
  "The ORIGINAL A5 curve (the shiftDigits construction), expressed on top of
  the L-system machinery: bit-for-bit compatible with the pre-L-system
  library.

  The old construction is two layers, and both are preserved here exactly:

  1. The base ordering is a simple two-motif L-system (the original
     quaternary curve the A5 curve grew out of), re-gauged into the 180°-only
     form the table compiler requires:
       W: W+++Z---WZ   Z: Z+++W---ZW    (draws W -> E, Z -> +e-)
  2. On top of it, the shiftDigits digit recode, which rearranges children so
     they overlap their parent cells.

  Orientations follow the old engine exactly: reverse remaps s -> N-1-s,
  flipIJ (uw/wu) selects the flipped pattern and mirrors the raw cell (x <-> z
  in triple space), invertJ (vw/wv) flips the quintant vertically ((x,y,z) ->
  (y-(n-1), x+(n-1), z), n = 2^res). Both maps are self-inverse.

  Digits are vectors, least significant first, and flips a pair [f0 f1] of
  ±1; A5 mutates both in place, so the functions here return them."
  (:require [dodeca.lattice.lsystem.index :refer [axiom-leaf-cell axiom-target->s ab->triple triple->ab]]
            [dodeca.lattice.lsystem.tables :refer [compile-grammar pow2]]))

(def ^:private original
  "The compiled two-motif grammar of the original curve (W/Z gauge)."
  (compile-grammar (array-map \W "W+++Z---WZ" \Z "Z+++W---ZW") (array-map \W "E" \Z "+e-")))

(def ^:private axiom-w (get (:motif-idx original) \W))

;; ---------- shiftDigits (ported verbatim from the original construction) ----------
;; Patterns used to rearrange the cells when shifting. This adjusts the layout
;; so that children always overlap with their parent cells.
(defn- reverse-pattern [pattern]
  (mapv (fn [i] (first (keep-indexed (fn [j p] (when (= p i) j)) pattern))) (range (count pattern))))

(def ^:private pattern [0 1 3 4 5 6 7 2])
(def ^:private pattern-flipped [0 1 2 7 3 4 5 6])
(def ^:private pattern-reversed (reverse-pattern pattern))
(def ^:private pattern-flipped-reversed (reverse-pattern pattern-flipped))

(defn- shift-digits
  "The digits with digits i-1 and i shifted by the pattern, where the flips
  and invertJ call for it."
  [digits i [f0 f1] invert-j pattern]
  (if (<= i 0)
    digits
    (let [parent-k (nth digits i 0)
          child-k (nth digits (dec i))
          F (+ f0 f1)
          ;; The value of F which cells need to be shifted. The rule is flipped
          ;; depending on the orientation, specifically on the value of invertJ
          [needs-shift first] (if (not= invert-j (= F 0))
                                ;; Second & third pentagons only; second pentagon is first
                                [(or (= parent-k 1) (= parent-k 2)) (= parent-k 1)]
                                ;; First two pentagons only; first pentagon is first
                                [(< parent-k 2) (= parent-k 0)])]
      (if-not needs-shift
        digits
        ;; Apply the pattern by setting the digits based on the value provided
        (let [src (if first child-k (+ child-k 4))
              dst (nth pattern src)]
          (assoc digits
                 (dec i) (mod dst 4)
                 i (mod (- (+ (+ parent-k 4) (quot dst 4)) (quot src 4)) 4)))))))

(defn- apply-digit-flips
  "The flips product accumulates per digit exactly as quaternaryToFlips did:
  digit 1 flips the second component, digit 3 the first."
  [[f0 f1 :as flips] d]
  (case d
    1 [f0 (- f1)]
    3 [(- f0) f1]
    flips))

(defn- forward-shift
  "Old s digits -> geometric (X/Y curve) digits: [digits flips]. The flips are
  the product over the shifted digits: the old engine's anchor state, from
  which the pentagon flavor follows in closed form (see compat-flavor)."
  [digits invert-j flip-ij]
  (let [pattern (if flip-ij pattern-flipped pattern)]
    (loop [i (dec (count digits))
           digits digits
           flips [1 1]]
      (if (neg? i)
        [digits flips]
        (let [digits (shift-digits digits i flips invert-j pattern)]
          (recur (dec i) digits (apply-digit-flips flips (nth digits i))))))))

(defn- inverse-shift
  "Geometric (X/Y curve) digits -> old s digits. The flips start as the
  product over ALL digits and each iteration cancels digit i's contribution,
  so at step i they hold the product of the digits ABOVE i, matching the
  forward pass's state at the same level."
  [digits invert-j flip-ij]
  (let [pattern (if flip-ij pattern-flipped-reversed pattern-reversed)
        flips (reduce apply-digit-flips [1 1] digits)]
    (loop [i 0
           digits digits
           flips flips]
      (if (= i (count digits))
        digits
        (let [flips (apply-digit-flips flips (nth digits i))]
          (recur (inc i) (shift-digits digits i flips invert-j pattern) flips))))))

(defn- digits-of [s resolution]
  (loop [v s
         digits []]
    (if (or (pos? v) (< (count digits) resolution))
      (recur (unsigned-bit-shift-right v 2) (conj digits (bit-and v 3)))
      digits)))

(defn- pack-digits [digits]
  (reduce (fn [s d] (bit-or (bit-shift-left s 2) d)) 0 (rseq (vec digits))))

;; ---------- orientations (as in the old engine) ----------
(def ^:private compat-orient
  {:uv {:reverse false :invert-j false :flip-ij false}
   :vu {:reverse true :invert-j false :flip-ij false}
   :uw {:reverse false :invert-j false :flip-ij true}
   :wu {:reverse true :invert-j false :flip-ij true}
   :vw {:reverse true :invert-j true :flip-ij false}
   :wv {:reverse false :invert-j true :flip-ij false}})

(defn- compat-flavor
  "Pentagon flavor from the old engine's anchor state: the flips product over
  the (shifted) digits and the leaf digit `q`. Flavor bit 0 (180° rotation)
  fires iff flips[1] is -1; bit 1 (Y reflection) on the (F, q) predicate
  below."
  [[f0 f1] q]
  (let [rotate (if (= f1 -1) 1 0)
        F (+ f0 f1)
        ;; Orient last two pentagons when both or neither flips are set,
        ;; first & last pentagons when exactly one is.
        reflect (if (if (= F 0) (or (= q 0) (= q 3)) (or (= q 2) (= q 3))) 1 0)]
    (bit-or rotate (bit-shift-left reflect 1))))

(defn- compat-descend
  "Shared forward descent: old s digits -> {:triple :flips :q}, the anchor
  flips and the leaf digit."
  [s resolution rec]
  (let [n (bit-shift-left 1 (* 2 resolution))
        v (if (:reverse rec) (- (- n 1) s) s)
        [digits flips] (forward-shift (digits-of v resolution) (:invert-j rec) (:flip-ij rec))
        raw (axiom-leaf-cell original (pack-digits digits) resolution axiom-w)
        triple (ab->triple (:a raw) (:b raw))
        triple (if (:flip-ij rec) {:x (:z triple) :y (:y triple) :z (:x triple)} triple)
        triple (if (:invert-j rec)
                 (let [n1 (dec (nth pow2 resolution))]
                   {:x (- (:y triple) n1) :y (+ (:x triple) n1) :z (:z triple)})
                 triple)]
    {:triple triple :flips flips :q (if (pos? (count digits)) (nth digits 0) 0)}))

(defn compat-s->triple
  "Old-curve position `s` -> triple coordinate, via the ORIGINAL (W/Z)
  forward descent + shiftDigits recode."
  ([s resolution] (compat-s->triple s resolution :uv))
  ([s resolution orientation]
   (:triple (compat-descend s resolution (get compat-orient orientation)))))

(defn compat-s->cell
  "Old-curve position `s` -> cell (triple + pentagon flavor)."
  ([s resolution] (compat-s->cell s resolution :uv))
  ([s resolution orientation]
   (let [rec (get compat-orient orientation)
         {:keys [triple flips q]} (compat-descend s resolution rec)
         ;; As in the old engine's sToAnchor: invertJ flips the first component
         ;; (flipIJ leaves the flips untouched).
         flips (if (:invert-j rec) [(- (first flips)) (second flips)] flips)]
     {:triple triple :flavor (compat-flavor flips q)})))

(defn compat-triple->s
  "Triple -> old-curve position `s`, or nil if the triple has invalid parity."
  ([t resolution] (compat-triple->s t resolution :uv))
  ([{:keys [x y z] :as t} resolution orientation]
   (let [sum (+ x y z)]
     (when (or (= sum 0) (= sum 1))
       (let [n (bit-shift-left 1 (* 2 resolution))
             rec (get compat-orient orientation)
             raw (if (:invert-j rec)
                   (let [n1 (dec (nth pow2 resolution))]
                     {:x (- y n1) :y (+ x n1) :z z})
                   t)
             raw (if (:flip-ij rec) {:x (:z raw) :y (:y raw) :z (:x raw)} raw)
             ab (triple->ab raw)
             s-geo (first (axiom-target->s original (:a ab) (:b ab) resolution axiom-w))
             v (pack-digits (inverse-shift (digits-of s-geo resolution) (:invert-j rec) (:flip-ij rec)))]
         (if (:reverse rec) (- (- n 1) v) v))))))

;; ---------- fractional point-location (ported verbatim from the original engine) ----------
;; The old engine located a fractional point with a few sign tests per level
;; (IJToQuaternary), bit-identical by construction including its boundary
;; tie-breaks. The resulting digit stream is the geometric (X/Y curve) digit
;; stream, so the same inverse-shift recode applies on top.

(defn- ij->quaternary
  "Which of the 4 children contains the scaled offset, under the current flips
  (the old engine's IJToQuaternary, verbatim)."
  [u v [f0 f1]]
  ;; Boundaries to compare against
  (let [a (if (= f0 -1) (- (+ u v)) (+ u v))
        b (if (= f1 -1) (- u) u)
        c (if (= f0 -1) (- v) v)]
    (if (= (+ f0 f1) 0)
      ;; Only one flip
      (cond (< c 1) 0 (> b 1) 3 (> a 1) 2 :else 1)
      ;; No flips or both
      (cond (< a 1) 0 (> b 1) 3 (> c 1) 2 :else 1))))

;; Child anchor offsets in IJ units, indexed by [flip combination][digit]
;; (= the old engine's KJToIJ(quaternaryToKJ(digit, flips))).
;; Flip index = (flips[0] === YES) + 2 * (flips[1] === YES).
(def ^:private child-offset-ij
  [[[0 0] [1 0] [0 1] [1 1]]      ; (NO, NO):   p = k, q = j
   [[0 0] [1 -1] [0 -1] [1 -2]]   ; (YES, NO):  p = -j, q = -k
   [[0 0] [-1 1] [0 1] [-1 2]]    ; (NO, YES):  p = j, q = k
   [[0 0] [-1 0] [0 -1] [-1 -1]]]) ; (YES, YES): p = -k, q = -j

(defn compat-ij->s
  "Fractional IJ point -> old-curve position `s` of the containing cell."
  ([ij resolution] (compat-ij->s ij resolution :uv))
  ([[i j] resolution orientation]
   (let [n (bit-shift-left 1 (* 2 resolution))
         rec (get compat-orient orientation)
         [i j] (if (:flip-ij rec) [j i] [i j])
         j (if (:invert-j rec) (- (nth pow2 resolution) (+ i j)) j)
         ;; Geometric digits by direct point-location, most significant first.
         digits (loop [lvl (dec resolution)
                       digits (vec (repeat resolution 0))
                       flips [1 1]
                       pivot-i 0
                       pivot-j 0]
                  (if (neg? lvl)
                    digits
                    (let [scale (/ 1.0 (nth pow2 lvl))
                          digit (ij->quaternary (* (- i pivot-i) scale) (* (- j pivot-j) scale) flips)
                          fi (+ (if (= (first flips) -1) 1 0) (if (= (second flips) -1) 2 0))
                          [oi oj] (get-in child-offset-ij [fi digit])]
                      (recur (dec lvl)
                             (assoc digits lvl digit)
                             (apply-digit-flips flips digit)
                             (+ pivot-i (* oi (nth pow2 lvl)))
                             (+ pivot-j (* oj (nth pow2 lvl)))))))
         v (pack-digits (inverse-shift digits (:invert-j rec) (:flip-ij rec)))]
     (if (:reverse rec) (- (- n 1) v) v))))
