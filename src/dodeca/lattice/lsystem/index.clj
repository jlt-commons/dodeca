;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.lattice.lsystem.index
  "The A5 space-filling curve, a turtle L-system on the triangular lattice.

  The curve is a vertex-to-vertex turtle L-system on the triangular lattice:
  7 self-referential motifs (A B C M P Q R), each a clean A5 unit (2
  parallelograms + 2 triangles). The symbolic grammar lives in grammar and is
  compiled to flat tables in tables; this namespace evaluates it as an
  O(resolution) digit transducer:
    forward  s -> cell   : descend the quaternary digits, accumulating a
             turtle position + heading, then map (a,b) -> A5 triple via a
             fixed similarity; the leaf state also yields the cell's pentagon
             flavor.
    inverse  triple -> s : descend picking, at each level, the child whose
             convex footprint (triforce / parallelogram) contains the target
             cell.

  Every turn in every rule is 180° (see tables), so the descent tracks
  orientation as a single flip bit.

  The 6 orientations are just which triforce motif is the axiom (uv->A,
  uw->C, vw->B) + whether the curve is reversed; A and C fill the quintant
  from corner C0, B fills it corner-to-corner so it's translated onto the
  quintant. See `orient`.

  A5 splits s into two float halves to keep the per-level work in plain
  numbers; s fits a long here (4^30 = 2^60), so its digits are read off
  directly. A cell is {:triple {:x :y :z} :flavor}; a descent state
  (CurveNode) is {:motif :flip :pos-a :pos-b}."
  (:require [dodeca.lattice.lsystem.grammar :refer [rules draws]]
            [dodeca.lattice.lsystem.tables :refer [compile-grammar pow2]]))

(def ^:private A5
  "The compiled A5 grammar."
  (compile-grammar rules draws))

;; A real cell's corner sum lies exactly on a separator (0, exact) or at least
;; 48 corner-sum units off it, at every level: the gap is between integer
;; points and fixed lines, so it does not shrink as the scale grows. A
;; threshold scaled with the level (as the footprints' bsp-eps is) would
;; swallow that gap by level 28 and pick the wrong child.
(def ^:private classify-threshold -1)

(defn- classify
  "Branchless child pick: 3 separator dot products form a 3-bit pattern that
  indexes the per-state lookup table. Used on the exact path, where the target
  is strictly interior at every level."
  ^long [^longs class-sep ^longs class-lut ^long state ^long rel-a ^long rel-b ^long scale]
  (let [b (* state 9)
        thr classify-threshold
        b0 (if (>= (+ (+ (* (aget class-sep b) rel-a) (* (aget class-sep (+ b 1)) rel-b))
                      (* (aget class-sep (+ b 2)) scale))
                   thr) 1 0)
        b1 (if (>= (+ (+ (* (aget class-sep (+ b 3)) rel-a) (* (aget class-sep (+ b 4)) rel-b))
                      (* (aget class-sep (+ b 5)) scale))
                   thr) 1 0)
        b2 (if (>= (+ (+ (* (aget class-sep (+ b 6)) rel-a) (* (aget class-sep (+ b 7)) rel-b))
                      (* (aget class-sep (+ b 8)) scale))
                   thr) 1 0)]
    (aget class-lut (+ (* state 8) (bit-or b0 (bit-or (bit-shift-left b1 1) (bit-shift-left b2 2)))))))

(defn- digit
  "The quaternary digit of s at position idx (0 least significant)."
  ^long [^long s ^long idx]
  (bit-and (unsigned-bit-shift-right s (* 2 idx)) 3))

;; ---------- exact (a,b) corner-sum <-> A5 triple ----------
;; The turtle (a,b) lattice and A5's triple frame are two views of the same
;; triangular grid. Composing them, the √3 from each basis cancels, leaving an
;; exact rational map: from a cell's corner sum (= 3·centroid),
;;   y - z      = (2·sum.a + sum.b - 12) / 12
;;   2x - y - z = (sum.b + 4) / 4
;; and the parity x+y+z ∈ {0,1} pins x, y, z. No floating point.

(defn- ab->triple*
  "`ab->triple` for a corner sum whose parity is known: no search and no
  checks. The quotients are exact for a real cell."
  [sum-a sum-b parity]
  (let [x (quot (+ (quot (+ sum-b 4) 4) parity) 3)
        r (- parity x)                              ; y + z
        yz (quot (- (+ (* 2 sum-a) sum-b) 12) 12)]  ; y - z
    {:x x :y (quot (+ r yz) 2) :z (quot (- r yz) 2)}))

(defn ab->triple [sum-a sum-b]
  (when (or (not (zero? (rem (+ (* 2 sum-a) sum-b) 12))) (not (zero? (rem sum-b 4))))
    (throw (ex-info (str "abToTriple: off-lattice corner sum (" sum-a "," sum-b ")") {:a sum-a :b sum-b})))
  ;; x = (2x - y - z + parity) / 3 is an integer, which pins the parity
  (let [parity (mod (- (quot (+ sum-b 4) 4)) 3)
        {:keys [x y z] :as triple} (ab->triple* sum-a sum-b parity)]
    (when (or (> parity 1) (not= (+ x y z) parity))
      (throw (ex-info (str "abToTriple: no integer triple for (" sum-a "," sum-b ")") {:a sum-a :b sum-b})))
    triple))

(defn triple->ab [{:keys [x y z]}]
  (let [b (- (* 4 (- (- (* 2 x) y) z)) 4)]
    {:a (quot (- (+ (* 12 (- y z)) 12) b) 2) :b b}))

;; ---------- forward: s -> leaf host cell (corner sum + flavor) ----------

(defn axiom-leaf-cell
  "The leaf host cell of s under `axiom`: its corner sum {:a :b} and :flavor.
  A child placed at (parent-relative) offset under a `flip` frame has its
  offset negated when flipped (180°); the child's own frame is `flip XOR
  child.flip`. Also used by dodeca.lattice.compat."
  [t s R axiom]
  (let [{:keys [child-token child-flip child-off-a child-off-b leaf-sum leaf-flavor]} (:arrays t)
        ^longs child-token child-token
        ^longs child-flip child-flip
        ^longs child-off-a child-off-a
        ^longs child-off-b child-off-b
        ^longs leaf-sum leaf-sum
        ^longs leaf-flavor leaf-flavor
        s (long s)]
    (loop [L (long R)
           motif (long axiom)
           flip 0
           pos-a 0
           pos-b 0]
      (if (>= L 2)
        (let [ci (+ (* motif 4) (digit s (dec L)))
              p (bit-shift-left 1 (- L 2))
              scale (if (pos? flip) (- p) p)]
          (recur (dec L)
                 (aget child-token ci)
                 (bit-xor flip (aget child-flip ci))
                 (+ pos-a (* (aget child-off-a ci) scale))
                 (+ pos-b (* (aget child-off-b ci) scale))))
        ;; level 1: leaf walk (from heading 0 or 3), take the d0-th host cell
        (let [d0 (if (>= R 1) (bit-and s 3) 0)
              base (+ (* motif 2) flip)]
          {:a (+ (* 3 pos-a) (aget leaf-sum (+ (* base 8) (* d0 2))))
           :b (+ (* 3 pos-b) (aget leaf-sum (+ (+ (* base 8) (* d0 2)) 1)))
           :flavor (aget leaf-flavor (+ (* base 4) d0))})))))

(defn- step-below
  "The descent state below the child with (axiom-order) digit `d` of the cell
  whose descent state is (motif, flip, pos-a, pos-b)."
  [t motif flip pos-a pos-b d]
  (let [ci (+ (* motif 4) d)
        sign (if (pos? flip) -1 1)]
    {:motif (nth (:child-token t) ci)
     :flip (bit-xor flip (nth (:child-flip t) ci))
     :pos-a (+ (* 2 pos-a) (* (nth (:child-off-a t) ci) sign))
     :pos-b (+ (* 2 pos-b) (* (nth (:child-off-b t) ci) sign))}))

;; ---------- inverse: descend by the branchless child classifier ----------

(defn axiom-target->s
  "The curve position of the cell whose corner sum is (ta, tb) under `axiom`:
  [s leaf-flavor node], node the descent state below the leaf (see
  `curve-child`). The target is the corner sum of a real cell, which is
  strictly interior at every level, so the classifier is the containing child
  and the leaf resolves by exact sum match. Also used by
  dodeca.lattice.compat."
  [t ta tb R axiom]
  (let [{:keys [child-token child-flip child-off-a child-off-b leaf-sum leaf-flavor class-sep class-lut]} (:arrays t)
        ^longs child-token child-token
        ^longs child-flip child-flip
        ^longs child-off-a child-off-a
        ^longs child-off-b child-off-b
        ^longs leaf-sum leaf-sum
        ^longs leaf-flavor leaf-flavor
        ta (long ta)
        tb (long tb)]
    (loop [L (long R)
           motif (long axiom)
           flip 0
           pos-a 0
           pos-b 0
           s 0]
      (if (>= L 2)
        (let [scale (bit-shift-left 1 (- L 2))
              sign (if (pos? flip) (- scale) scale)
              best-d (classify class-sep class-lut (+ (* motif 2) flip) (- ta (* 3 pos-a)) (- tb (* 3 pos-b)) scale)
              ci (+ (* motif 4) best-d)]
          (recur (dec L)
                 (aget child-token ci)
                 (bit-xor flip (aget child-flip ci))
                 (+ pos-a (* (aget child-off-a ci) sign))
                 (+ pos-b (* (aget child-off-b ci) sign))
                 (bit-or s (bit-shift-left best-d (* 2 (dec L))))))
        ;; level 1: pick the leaf cell by exact corner-sum match
        (let [base (+ (* motif 2) flip)
              rel-a (- ta (* 3 pos-a))
              rel-b (- tb (* 3 pos-b))
              d0 (loop [d 0]
                   (cond
                     (= d 4) -1
                     (and (= (aget leaf-sum (+ (* base 8) (* d 2))) rel-a)
                          (= (aget leaf-sum (+ (+ (* base 8) (* d 2)) 1)) rel-b)) d
                     :else (recur (inc d))))]
          (when (neg? d0)
            (throw (ex-info (str "lsystem inverse: no leaf match for corner sum (" ta "," tb ")") {:a ta :b tb})))
          [(bit-or s d0)
           (aget leaf-flavor (+ (* base 4) d0))
           (step-below t motif flip pos-a pos-b d0)])))))

;; ---------- orientation = which triforce motif is the axiom ----------
;; Each orientation is one of the three triforce motifs used as the axiom
;; (uv->A, uw->C, wv->B), with the reverse orientations (vu, wu, vw) walking
;; the same curve backward (s -> N-1-s). A and C fill the quintant from corner
;; C0 (tau 0); B fills it corner-to-corner (C1<->C2), so it's translated onto
;; the quintant by tau = (-p, p, 0) in triple units, p = 2^resolution, which
;; is (+12p, -12p) in the corner-sum frame.
(def ^:private orient
  (let [idx (:motif-idx A5)]
    {:uv {:axiom (get idx \A) :reverse false :is-b false}
     :vu {:axiom (get idx \A) :reverse true :is-b false}
     :uw {:axiom (get idx \C) :reverse false :is-b false}
     :wu {:axiom (get idx \C) :reverse true :is-b false}
     :vw {:axiom (get idx \B) :reverse true :is-b true}
     :wv {:axiom (get idx \B) :reverse false :is-b true}}))

(def level0-flavor
  "The flavor of the single cell at resolution 0. There the leaf is the axiom
  itself, whose flavor reflects the axiom rather than the cell, so it is fixed
  instead. The single cell is the quintant's corner cell [-maxRow, maxRow, 0],
  which touches a dodecahedron vertex and is flavor 2 at every resolution, so
  the corner cells keep one orientation all the way down."
  2)

(defn s->cell
  "The A5 curve position `s` -> cell (triple coordinate + pentagon flavor), for
  a given resolution and orientation (default :uv). The triple is bijective
  with `triple->s-lattice`."
  ([s resolution] (s->cell s resolution :uv))
  ([s resolution orientation]
   (if (zero? resolution)
     {:triple {:x 0 :y 0 :z 0} :flavor level0-flavor}
     (let [n (bit-shift-left 1 (* 2 resolution))
           rec (get orient orientation)
           s-axiom (if (:reverse rec) (- (- n 1) s) s)
           cell (axiom-leaf-cell A5 s-axiom resolution (:axiom rec))
           base (ab->triple (:a cell) (:b cell))]
       (if-not (:is-b rec)
         {:triple base :flavor (:flavor cell)}
         (let [p (nth pow2 resolution)]
           {:triple {:x (- (:x base) p) :y (+ (:y base) p) :z (:z base)} :flavor (:flavor cell)}))))))

(defn s->triple
  "The A5 curve position `s` -> triple coordinate, for a given resolution and
  orientation. Bijective with `triple->s-lattice`."
  ([s resolution] (s->triple s resolution :uv))
  ([s resolution orientation] (:triple (s->cell s resolution orientation))))

(defn- triple->curve
  "`triple->s-lattice`, also giving the leaf flavor and the descent state
  below the cell: [s flavor node]."
  [triple resolution orientation]
  (let [n (bit-shift-left 1 (* 2 resolution))
        rec (get orient orientation)
        ab (triple->ab triple)
        tau-sum (if (:is-b rec) (* 12 (nth pow2 resolution)) 0)
        [s flavor node] (axiom-target->s A5 (- (:a ab) tau-sum) (+ (:b ab) tau-sum) resolution (:axiom rec))]
    [(if (:reverse rec) (- (- n 1) s) s) flavor node]))

(defn triple->s-lattice
  "Triple coordinate -> the A5 curve position `s`, for a given resolution and
  orientation. Inverse of `s->triple`."
  ([triple resolution] (triple->s-lattice triple resolution :uv))
  ([triple resolution orientation] (first (triple->curve triple resolution orientation))))

;; ---------- stepwise descent: the cell hierarchy in curve order ----------
;; A walk down the hierarchy reads one digit per level, so rather than a full
;; O(resolution) descent per cell it carries each cell's descent state and
;; takes each child in O(1). The state below a cell is the one its children's
;; digits are read from: the motif, flip and turtle position after the cell's
;; own digits, the position in units of the children's level (one level down,
;; it doubles).

;; The parity (x + y + z) of each leaf cell by (motif, flip, digit): a cell's
;; position only translates its leaf by lattice vectors, which keep the parity.
(def ^:private leaf-parity
  (let [leaf-sum (:leaf-sum A5)]
    (mapv (fn [i]
            (let [{:keys [x y z]} (ab->triple (nth leaf-sum (* 2 i)) (nth leaf-sum (inc (* 2 i))))]
              (+ x y z)))
          (range (count (:leaf-flavor A5))))))

(defn curve-child
  "The child with curve digit `digit` (0-3, the child's last digit of s) of
  the cell whose descent state is `node`: its :triple at `resolution` (the
  child's), its :flavor, and the descent state below it as :node. Agrees with
  `s->cell` on the child's s."
  [{:keys [motif flip pos-a pos-b]} digit resolution orientation]
  (let [{:keys [leaf-sum leaf-flavor]} A5
        rec (get orient orientation)
        ;; A reversed curve reads s as N - 1 - s: every digit complemented
        d (if (:reverse rec) (- 3 digit) digit)
        base (+ (* motif 2) flip)
        ;; The parity is known up front (see leaf-parity): no search
        sum-a (+ (* 3 pos-a) (nth leaf-sum (+ (* base 8) (* d 2))))
        sum-b (+ (* 3 pos-b) (nth leaf-sum (+ (* base 8) (* d 2) 1)))
        triple (ab->triple* sum-a sum-b (nth leaf-parity (+ (* base 4) d)))
        triple (if (:is-b rec)
                 (-> triple
                     (update :x - (nth pow2 resolution))
                     (update :y + (nth pow2 resolution)))
                 triple)]
    {:triple triple
     :flavor (nth leaf-flavor (+ (* base 4) d))
     :node (step-below A5 motif flip pos-a pos-b d)}))

(defn triple->curve-node
  "A cell's curve position :s, :flavor and the descent state below it as
  :node, from its triple: where a walk down to the cell by `curve-child`
  would arrive, in one descent rather than one step per level."
  [triple resolution orientation]
  (if (zero? resolution)
    ;; Below the resolution-0 cell (the whole quintant) is the axiom itself
    {:s 0 :flavor level0-flavor :node {:motif (:axiom (get orient orientation)) :flip 0 :pos-a 0 :pos-b 0}}
    (let [[s flavor node] (triple->curve triple resolution orientation)]
      {:s s :flavor flavor :node node})))
