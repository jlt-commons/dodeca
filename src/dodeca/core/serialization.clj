;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.serialization
  "Cell IDs: their bit layout, the leaf slots they stand for, and the walk up
  and down the hierarchy.

  A cell is {:origin :segment :s :resolution}, A5's A5Cell with S spelled
  `:s`. IDs and slots are longs holding A5's unsigned 64 bits; WORLD_SLOTS,
  ORIGIN_SLOTS and every res-0 tag past origin 6 read negative, so every
  comparison of them goes through `u<`/`u<=` and every shift right is logical.
  Math.log2 of a tag, which is exact on a power of two, is its trailing-zero
  count."
  (:require [dodeca.core.origin :as origin]
            [dodeca.utils.unsigned :as u :refer [ushr u< u<=]]))

(def first-curve-resolution 2)
(def max-resolution 30)
;; IDs below res 30 start with a 6-bit origin (res 0) or quintant, above S
(def quintant-shift 58)
(def s-mask (dec (bit-shift-left 1 quintant-shift)))

(def world-cell
  "Abstract cell that contains the whole world, has resolution -1 and 12
  children, which are the res0 cells."
  0)

;; Resolution 30 IDs have no room for a 6-bit quintant: its field is 5, 3 or 1
;; bits wide, marked by the tag (lowest bits) ...1, ...100 or ...10000:
;;   ...1     -> 5-bit quintant (0-31),  58-bit S
;;   ...100   -> 3-bit quintant (32-39), 58-bit S
;;   ...10000 -> 1-bit quintant (40-41), 58-bit S
;; Quintants 42-59 have no res-30 IDs.
(def res30-quintants
  "The number of quintants (in ID order) with resolution 30 IDs."
  42)

(defn- res30->slot
  "The leaf slot of a res-30 ID: its quintant, then its 58-bit S."
  [index]
  (cond
    (not (zero? (bit-and index 1)))
    (bit-or (bit-shift-left (ushr index 59) quintant-shift) (bit-and (ushr index 1) s-mask))
    (not (zero? (bit-and index 2r100)))
    (bit-or (bit-shift-left (+ (ushr index 61) 32) quintant-shift) (bit-and (ushr index 3) s-mask))
    :else
    (bit-or (bit-shift-left (+ (ushr index 63) 40) quintant-shift) (bit-and (ushr index 5) s-mask))))

(defn- slot->res30
  "The res-30 ID of a leaf slot in quintants 0-41."
  [slot]
  (let [q (ushr slot quintant-shift)
        s (bit-and slot s-mask)]
    (cond
      (< q 32) (bit-or (bit-shift-left q 59) (bit-shift-left s 1) 1)
      (< q 40) (bit-or (bit-shift-left (- q 32) 61) (bit-shift-left s 3) 2r100)
      :else (bit-or (bit-shift-left (- q 40) 63) (bit-shift-left s 5) 2r10000))))

;; Leaf slots. The A5 curve, at resolution 30, passes through every leaf
;; (res-30) cell of the globe once: picture it as a line of slots, one per leaf
;; cell, numbered in curve order from 0 to 60 * 4^29 - 1. A leaf slot is the
;; 6-bit quintant (0-59) then the leaf's S, left-aligned below it.
;;
;; A cell at resolution r occupies 4^(30-r) consecutive slots, an aligned block
;; starting at its first slot, and the cells of a resolution step along the
;; slots in strides of that size, as the curve does. Unlike cell IDs, whose
;; layout differs at resolutions 0, 1 and 30, slots put every cell on one
;; integer line in curve order, including all 60 quintants at resolution 30.
;; A leaf slot is not a cell ID.

(def quintant-slots (bit-shift-left 1 quintant-shift))
(def origin-slots (unchecked-multiply 5 quintant-slots))
(def world-slots (unchecked-multiply 60 quintant-slots))

(def resolution-tags
  "By resolution 0..30: the resolution tag (lowest set bit) of a cell below
  res 30."
  (mapv (fn [r]
          (case r
            0 (bit-shift-left 1 57)
            1 (bit-shift-left 1 56)
            (bit-shift-left 1 (max (- 59 (* 2 r)) 0))))
        (range (inc max-resolution))))

(def slot-counts
  "By resolution 0..30: the number of slots a cell occupies."
  (mapv (fn [r]
          (case r
            0 origin-slots
            1 quintant-slots
            (bit-shift-left 1 (- 60 (* 2 r)))))
        (range (inc max-resolution))))

(def res30-tag-bits
  "Res-30 IDs end in ...1, ...100 or ...10000: their tag has one of these bits"
  2r10101)

(def ^:private curve-tag-bits
  "The tags of resolutions 2-29: the odd bits 55 down to 1"
  (reduce bit-or 0 (subvec resolution-tags first-curve-resolution max-resolution)))

(defn- invalid-cell [cell]
  (ex-info (str "Invalid cell: 0x" (Long/toHexString cell)) {:cell cell}))

(defn cell-first-slot
  "The first slot a cell occupies. Throws if the value is not an A5 cell ID:
  its tag (lowest set bit) must be a resolution tag, and its origin (res 0) or
  quintant (res 1-29) must exist. Every res-30 pattern decodes to an existing
  quintant (0-41)."
  [cell]
  ;; The resolution tag: the lowest set bit (0 for the world cell)
  (let [tag (u/lowest-set-bit cell)]
    (or (if (u< tag (nth resolution-tags 1))
          (cond
            (not (zero? (bit-and tag res30-tag-bits))) (res30->slot cell)
            ;; Resolutions 2-29, in quintants 0-59: the first slot is the ID without its tag
            (and (not (zero? (bit-and tag curve-tag-bits))) (u< cell world-slots)) (unchecked-subtract cell tag)
            (zero? tag) 0)
          ;; Resolution 0 (tag bit 57) starts its origin's 5 quintants, 1 (bit 56) its quintant
          (let [top (ushr cell quintant-shift)]
            (cond
              (and (= tag (nth resolution-tags 0)) (< top 12)) (bit-shift-left (* 5 top) quintant-shift)
              (and (= tag (nth resolution-tags 1)) (< top 60)) (bit-shift-left top quintant-shift))))
        (throw (invalid-cell cell)))))

(defn cell-first-slot-unchecked
  "The first slot of a cell, without checking that the value is an A5 cell ID:
  for searches, which check the cell they land on. A value that is not a cell
  gives a meaningless slot."
  [cell]
  (let [tag (u/lowest-set-bit cell)]
    (cond
      (zero? tag) 0
      (u<= (nth resolution-tags 1) tag)
      (let [top (ushr cell quintant-shift)]
        (bit-shift-left (if (= tag (nth resolution-tags 0)) (* 5 top) top) quintant-shift))
      (not (zero? (bit-and tag res30-tag-bits))) (res30->slot cell)
      :else (unchecked-subtract cell tag))))

(defn valid-cell?
  "Check whether a 64-bit value is an A5 cell ID: its tag (lowest set bit) must
  be a resolution tag, and its origin (res 0) or quintant (res 1-29) must
  exist. The world cell counts as a cell: it has no boundary, but
  `cell->parent` gives it as the parent of the res-0 cells. A compaction
  marker is not a cell."
  [cell]
  ;; Every long is at most U64_MAX, and only zero is at most 0 unsigned
  (if (zero? cell)
    true
    (let [bit (u/trailing-zeros cell)]
      ;; Resolutions 2-29 (odd bits 55-1), in quintants 0-59, or 30 (bits 4, 2, 0)
      (if (< bit 56)
        (if (odd? bit) (u< cell world-slots) (<= bit 4))
        ;; Resolution 0 (bit 57) of origins 0-11, or 1 (bit 56) of quintants 0-59
        (let [top (ushr cell quintant-shift)]
          (or (and (= bit 57) (< top 12))
              (and (= bit 56) (< top 60))))))))

(defn checked-resolution
  "The resolution of a cell, as `get-resolution` gives it, but throwing if the
  value is not an A5 cell ID (see `valid-cell?`)."
  [cell]
  (if (zero? cell)
    -1
    (let [bit (u/trailing-zeros cell)]
      (or (if (< bit 56)
            (cond
              (and (odd? bit) (u< cell world-slots)) (bit-shift-right (- 59 bit) 1)
              (and (<= bit 4) (even? bit)) max-resolution)
            (let [top (ushr cell quintant-shift)]
              (cond
                (and (= bit 57) (< top 12)) 0
                (and (= bit 56) (< top 60)) 1)))
          (throw (invalid-cell cell))))))

(defn cell-slot-count
  "The number of slots a cell occupies."
  [cell]
  ;; The resolution tag: the lowest set bit (0 for the world cell)
  (let [tag (u/lowest-set-bit cell)]
    (cond
      (zero? tag) world-slots
      (= tag (nth resolution-tags 0)) origin-slots
      (= tag (nth resolution-tags 1)) quintant-slots
      (not (zero? (bit-and tag res30-tag-bits))) 1
      ;; Resolutions 2-29: the slots are symmetric about the ID
      :else (bit-shift-left tag 1))))

(defn slot->cell
  "The res-r cell whose block of slots starts at `slot`."
  [slot resolution]
  (cond
    (neg? resolution) world-cell
    (and (pos? resolution) (< resolution max-resolution)) (unchecked-add slot (nth resolution-tags resolution))
    (zero? resolution) (bit-or (bit-shift-left (quot (ushr slot quintant-shift) 5) quintant-shift)
                               (nth resolution-tags 0))
    :else (slot->res30 slot)))

(defn get-resolution
  [index]
  ;; The resolution tag: the lowest set bit (0 for the world cell). Its position
  ;; gives the resolution: bit 57 is res 0, 56 res 1, 59 - 2r res r (2-29), and
  ;; res 30 uses the patterns ...1, ...100 and ...10000 (bits 0, 2 and 4).
  (if (zero? index)
    -1
    (let [bit (u/trailing-zeros index)]
      (cond
        (= bit 57) 0
        (= bit 56) 1
        (and (<= bit 4) (even? bit)) max-resolution
        :else (bit-shift-right (- 59 bit) 1)))))

(defn deserialize
  [index]
  (let [resolution (get-resolution index)]
    ;; Technically not a resolution, but can be useful to think of as an
    ;; abstract cell that contains the whole world
    (if (= resolution -1)
      {:origin (nth origin/origins 0) :segment 0 :s 0 :resolution resolution}
      ;; The cell's first slot holds its quintant, then its S above the slots of one cell
      (let [slot (cell-first-slot index)
            quintant (ushr slot quintant-shift)
            o (nth origin/origins (quot quintant 5))]
        (if (zero? resolution)
          {:origin o :segment 0 :s 0 :resolution resolution}
          {:origin o
           :segment (mod (+ quintant (:first-quintant o)) 5)
           :s (if (< resolution first-curve-resolution)
                0
                (quot (bit-and slot s-mask) (nth slot-counts resolution)))
           :resolution resolution})))))

(defn serialize
  [{o :origin :keys [segment s resolution]}]
  (when (> resolution max-resolution)
    (throw (ex-info (str "Resolution (" resolution ") is too large") {:resolution resolution})))
  (cond
    (= resolution -1) world-cell
    (zero? resolution) (slot->cell (unchecked-multiply (* 5 (:id o)) quintant-slots) 0)
    :else
    ;; The cell's first slot: its quintant, then S cells of this resolution into it.
    ;; `*` promotes on overflow, so an S too large for the resolution compares as it does in A5.
    (let [offset (if (>= resolution first-curve-resolution) (* s (nth slot-counts resolution)) 0)]
      (when (>= offset quintant-slots)
        (throw (ex-info (str "S (" s ") is too large for resolution level " resolution)
                        {:s s :resolution resolution})))
      (let [quintant (+ (* 5 (:id o)) (mod (+ (- segment (:first-quintant o)) 5) 5))]
        ;; Quintants past res30-quintants have no res-30 IDs: fall back to res 29
        (if (and (= resolution max-resolution) (>= quintant res30-quintants))
          (serialize {:origin o :segment segment :s (bit-shift-right s 2) :resolution (dec max-resolution)})
          (slot->cell (+ (bit-shift-left quintant quintant-shift) offset) resolution))))))

;; The segments of an origin in ID (quintant) order, by its first-quintant
(def ^:private quintant-segments
  (mapv (fn [first] (mapv (fn [n] (mod (+ n first) 5)) (range 5))) (range 5)))

(defn cell->children
  "The children of a cell at `child-resolution` (default: the next
  resolution), in ascending ID order."
  ([index]
   (cell->children index nil))
  ([index child-resolution]
   (let [{o :origin :keys [segment s] current-resolution :resolution} (deserialize index)
         new-resolution (or child-resolution (inc current-resolution))]
     (when (< new-resolution current-resolution)
       (throw (ex-info (str "Target resolution (" new-resolution
                            ") must be equal to or greater than current resolution ("
                            current-resolution ")")
                       {:cell index :resolution new-resolution})))
     (when (> new-resolution max-resolution)
       (throw (ex-info (str "Target resolution (" new-resolution
                            ") exceeds maximum resolution (" max-resolution ")")
                       {:cell index :resolution new-resolution})))
     ;; If target resolution equals current resolution, return the original cell
     (if (= new-resolution current-resolution)
       [index]
       (let [new-origins (if (= current-resolution -1) origin/origins [o])
             all-segments (or (and (= current-resolution -1) (pos? new-resolution))
                              (zero? current-resolution))
             resolution-diff (- new-resolution (max current-resolution (dec first-curve-resolution)))
             ;; From the world cell to res 0 the difference is -1: 4^-1 children
             ;; round up to one per origin, and S is 0
             children-count (if (pos? resolution-diff) (bit-shift-left 1 (* 2 resolution-diff)) 1)
             shifted-s (if (pos? resolution-diff) (bit-shift-left s (* 2 resolution-diff)) s)]
         (vec (for [new-origin new-origins
                    ;; An origin's quintants in ID order: the n-th is segment (n + first-quintant) % 5
                    new-segment (if all-segments
                                  (nth quintant-segments (:first-quintant new-origin))
                                  [segment])
                    i (range children-count)]
                (serialize {:origin new-origin :segment new-segment
                            :s (+ shifted-s i) :resolution new-resolution}))))))))

(defn- max-resolution?
  "Whether a cell is at resolution 30: its tag is one of ...1, ...100 or ...10000."
  [index]
  (not (zero? (bit-and (u/lowest-set-bit index) res30-tag-bits))))

(defn- normalize-res30
  "Re-pack a res-30 cell into the standard res-29 bit layout (6-bit quintant
  in [63..58], 56-bit S in [57..2], tag at bit 1). The 58-bit res-30 S is
  truncated by 2 bits, exactly as `(cell->parent _ 29)` would."
  [index]
  ;; The res-29 parent starts at the same slot, rounded down to its 4 children
  (bit-or (bit-and (res30->slot index) (bit-not 3)) 2r10))

(defn- finer-target
  "Throws the error for a target resolution finer than the cell's own."
  [index parent-resolution]
  (throw (ex-info (str "Target resolution (" parent-resolution
                       ") must be equal to or less than current resolution ("
                       (get-resolution index) ")")
                  {:cell index :resolution parent-resolution})))

(defn- cell->parent* [index parent-resolution check]
  (cond
    ;; Special case: parent of resolution 0 cells is the world cell
    (= parent-resolution -1) world-cell
    (or (< parent-resolution -1) (> parent-resolution max-resolution))
    (throw (ex-info (str "Target resolution (" parent-resolution ") is out of range")
                    {:cell index :resolution parent-resolution}))
    :else
    (do
      ;; A cell coarser than the target has no parent there. Each branch below
      ;; detects that from bits it already reads.
      (when (= index world-cell) (finer-target index parent-resolution))
      ;; Normalize res-30 children to the standard res-29 layout. After this,
      ;; the fast paths below treat the cell as a curve-range cell.
      (let [res30 (max-resolution? index)
            c (if res30 (normalize-res30 index) index)]
        (cond
          (and res30 (= parent-resolution max-resolution)) index ; identity (already res 30)
          (and res30 (= parent-resolution (dec max-resolution))) c

          (>= parent-resolution first-curve-resolution)
          ;; Curve-range parent: clear bits below the parent tag, set the tag.
          ;; Identity falls out for free: the tag lands in the same position and
          ;; bits below the keep cut are already zero. A cell at the parent
          ;; resolution or finer has its tag below the cut; a coarser cell has no
          ;; bits there (at resolution 30 the cut is bit 0, so every cell not
          ;; already res 30 is coarser).
          (let [keep-shift (- 60 (* 2 parent-resolution))
                kept (bit-shift-left (ushr c keep-shift) keep-shift)]
            (when (and check (= kept c)) (finer-target index parent-resolution))
            (bit-or kept (bit-shift-left 1 (- 59 (* 2 parent-resolution)))))

          :else
          ;; A res-0 cell has bit 57 set with all lower bits zero
          (let [res0 (zero? (bit-and c (dec (bit-shift-left 1 57))))]
            (if (= parent-resolution 1)
              (do
                (when res0 (finer-target index parent-resolution))
                ;; Top 6 bits already encode 5*originId + segmentN; only the tag moves
                (bit-or (bit-shift-left (ushr c 58) 58) (bit-shift-left 1 56)))
              ;; parent-resolution 0: top 6 bits change from quintant (0-59) to
              ;; originId (0-11). A res-0 cell is its own parent.
              (if res0
                c
                (bit-or (bit-shift-left (quot (ushr c 58) 5) 58) (bit-shift-left 1 57))))))))))

(defn cell->parent
  "Walk a cell up the hierarchy to a coarser resolution (or its own, giving the
  cell). A finer resolution is an error.

  Pure bit operations over the encoded index, with no deserialize/serialize
  round trip. The three encoding regimes (non-curve res 0/1, curve res
  2..29, variable-width res 30) all reduce to the same shape after a small
  amount of normalization."
  ([index]
   ;; The immediate parent is never finer than the cell: only an explicit
   ;; target needs the check
   (cell->parent* index (dec (get-resolution index)) false))
  ([index parent-resolution]
   (if (nil? parent-resolution)
     (cell->parent index)
     (cell->parent* index parent-resolution true))))

;; The 12 resolution-0 cells (dodecahedron faces) are a constant
(def ^:private res0-cells (delay (cell->children world-cell 0)))

(defn get-res0-cells
  "Resolution 0 cells of the A5 system, which serve as a starting point for all
  higher-resolution subdivisions in the hierarchy: the 12 dodecahedron faces."
  []
  @res0-cells)

(defn child-of?
  "Bit-level descendant test: is `child` the same cell as `parent`, or one of
  its descendants at any deeper resolution? Compares the high (quintant +
  parent's curve) bits in a single shift.

  Restricted to the curve range: `parent-resolution` must be in
  [first-curve-resolution .. max-resolution - 1], and `child` must not be a
  resolution-30 cell (whose encoding uses a variable quintant shift). Callers
  handling those cases should fall back to `cell->parent` equality."
  [child parent parent-resolution]
  ;; Parent's identifying bits occupy positions 63..(60-2P): 6 quintant bits
  ;; + 2(P-1) curve bits. Bit (59-2P) is the tag, below that is zero.
  (let [shift (- 60 (* 2 parent-resolution))]
    (= (ushr child shift) (ushr parent shift))))
