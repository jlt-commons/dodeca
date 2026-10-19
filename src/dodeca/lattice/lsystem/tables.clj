;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.lattice.lsystem.tables
  "Compiles an L-system grammar into flat numeric tables, once at load, so
  the descents in index are pure scalar arithmetic: no string expansion per
  call.

  Every grammar compiled here keeps every turn inside a rule at 180°
  (`+++`/`---`), so a child is only ever placed un-flipped or flipped (rotated
  180°) relative to its parent, never at 60°. compile-grammar enforces that
  invariant and records the orientation as a single `flip` bit (180° = negate
  on this lattice); the whole descent then tracks orientation as one boolean.
  For the A5 grammar the invariant is also what keeps every parallelogram cell
  on-axis; the compat W/Z grammar is the original two-motif curve re-gauged
  into this form (see dodeca.lattice.compat).

  Motif tokens are indexed by their position in the motif list (uppercase
  motifs first, then their lowercase reverses); a descent state is (motif
  index, flip bit). The tables are vectors indexed by that state. Every entry
  is an integer, as A5's float64 entries are integer-valued, so they are
  longs here."
  (:require [dodeca.lattice.lsystem.grammar :refer [reverse-motif expand-once]]
            [dodeca.lattice.lsystem.turtle :as turtle]))

(def bsp-eps 1e-6)

(defn- child-polys
  "The 4 child footprint polygons for state (motif, pflip), normalised (scale-invariant)."
  [t motif pflip]
  (let [psign (if (pos? pflip) -1 1)]
    (mapv (fn [d]
            (let [ci (+ (* motif 4) d)
                  tok (nth (:child-token t) ci)
                  cfl (nth (:child-flip t) ci)
                  oa (nth (:child-off-a t) ci)
                  ob (nth (:child-off-b t) ci)
                  edges (nth (:fp-edges t) (+ (* tok 2) (bit-xor pflip cfl)))]
              [d (mapv (fn [e] [(+ (* (* 3 oa) psign) (nth edges e))
                                (+ (* (* 3 ob) psign) (nth edges (inc e)))])
                       (range 0 (count edges) 4))]))
          (range 4))))

(defn- split
  "Children on the positive and negative side of the line, or nil when one
  straddles it."
  [children nx ny c]
  (reduce (fn [[pos neg] [_ cp :as child]]
            (let [vals (map (fn [[x y]] (+ (+ (* nx x) (* ny y)) c)) cp)
                  mn (reduce min ##Inf vals)
                  mx (reduce max ##-Inf vals)]
              (cond
                (>= mn (- bsp-eps)) [(conj pos child) neg]
                (<= mx bsp-eps) [pos (conj neg child)]
                :else (reduced nil))))
          [[] []]
          children))

(defn- build-bsp
  "Build a child-selection BSP; children tile convexly, so a polygon edge
  cleanly splits them."
  [children]
  (if (= 1 (count children))
    {:leaf (first (first children))}
    (or (first (for [[_ poly] children
                     :let [n (count poly)]
                     i (range n)
                     :let [[x1 y1] (nth poly i)
                           [x2 y2] (nth poly (mod (inc i) n))
                           nx (- y2 y1)
                           ny (- (- x2 x1))
                           c (- (+ (* nx x1) (* ny y1)))
                           [pos neg :as sides] (split children nx ny c)]
                     :when (and sides (seq pos) (seq neg))]
                 {:nx nx :ny ny :c c :pos (build-bsp pos) :neg (build-bsp neg)}))
        (throw (ex-info "lsystem: no clean BSP split for child set" {})))))

(defn- collect-seps [tree seps]
  (if (contains? tree :leaf)
    seps
    (let [k [(:nx tree) (:ny tree) (:c tree)]
          seps (if (some #(= k %) seps) seps (conj seps k))]
      (collect-seps (:neg tree) (collect-seps (:pos tree) seps)))))

(defn- walk-bsp [tree p seps]
  (if (contains? tree :leaf)
    (:leaf tree)
    (let [k [(:nx tree) (:ny tree) (:c tree)]
          idx (first (keep-indexed (fn [i sep] (when (= sep k) i)) seps))]
      (walk-bsp (if (pos? (bit-and (bit-shift-right p idx) 1)) (:pos tree) (:neg tree)) p seps))))

;; The pentagon FLAVOR (0-3) of the cell a draw symbol hosts: which of the four
;; pentagon orientations of the Cairo-like metatile it gets. The pentagon is a
;; 1:1 function of the cell's jigsaw piece and reduces to the closed-form rule
;;   flavor = BASE[symbol] XOR isLowercase XOR (heading & 1)
;; with BASE = {S:0, D:1, E:2, T:3}; bit 0 is a 180° rotation, bit 1 a Y
;; reflection of the base pentagon (see dodeca.core.tiling). Derived and
;; verified exhaustively against the pentagon geometry.
(def ^:private flavor-base {\S 0 \D 1 \E 2 \T 3})

(defn- convex-hull [pts]
  (let [p (->> pts
               (reduce (fn [[seen out] q] (if (contains? seen q) [seen out] [(conj seen q) (conj out q)]))
                       [#{} []])
               second
               (sort-by (juxt :a :b)))
        cross (fn [o a b] (- (* (- (:a a) (:a o)) (- (:b b) (:b o)))
                             (* (- (:b a) (:b o)) (- (:a b) (:a o)))))
        chain (fn [pts]
                (reduce (fn [hull q]
                          (conj (loop [hull hull]
                                  (if (and (>= (count hull) 2)
                                           (<= (cross (nth hull (- (count hull) 2)) (peek hull) q) 0))
                                    (recur (pop hull))
                                    hull))
                                q))
                        []
                        pts))]
    (if (< (count p) 3)
      (vec p)
      (let [lower (chain p)
            upper (chain (reverse p))]
        (into (pop lower) (pop upper))))))

(defn compile-grammar
  "Compile a grammar (motif rules + leaf draws) into flat descent tables.
  Lowercase motifs are the uppercase rules reversed, generated automatically.

  The tables: :motif-idx; per child ci = motif * 4 + digit, :child-token,
  :child-flip, :child-off-a, :child-off-b; per (motif, flip), :fp-edges, the
  footprint hull as [3*c0.a 3*c0.b d.a d.b]*E, :leaf-sum (4 host cells as
  corner sums) and :leaf-flavor; and the branchless child classifier,
  :class-sep (3 separating lines [nx ny c] per state) and :class-lut (the
  child digit per 3-bit pattern). :arrays holds the same tables as long
  arrays."
  [rules draws]
  (let [motifs (vec (keys rules))
        all-motifs (into motifs (map #(Character/toLowerCase %) motifs))
        motif-count (count all-motifs)
        motif-idx (into {} (map-indexed (fn [i m] [m i]) all-motifs))
        ;; Expand a motif to a pure draw string: `level` rule passes, then one
        ;; draws pass (turning every remaining motif into its leaf terminal).
        to-draws (fn [motif level]
                   (expand-once (nth (iterate #(expand-once % rules) (str motif)) level) draws))
        motif-net (fn [motif] (:disp (turtle/net-of (to-draws motif 1))))
        ;; ---------- child tables: 4 children per motif ----------
        child-table (fn [rule]
                      (let [children (:children
                                      (reduce (fn [{:keys [pos h] :as state} ch]
                                                (cond
                                                  (= ch \+) (assoc state :h (mod (inc h) 6))
                                                  (= ch \-) (assoc state :h (mod (+ h 5) 6))
                                                  (not (contains? rules (Character/toUpperCase ch))) state
                                                  :else
                                                  (do
                                                    (when-not (or (= h 0) (= h 3))
                                                      (throw (ex-info (str "lsystem: non-180° turn (" (* 60 h)
                                                                           "°) before a child in rule \"" rule "\"")
                                                                      {:rule rule})))
                                                    (let [flip (= h 3)
                                                          n (motif-net ch)]
                                                      (-> state
                                                          (update :children conj {:token ch :off-unit pos :flip flip})
                                                          (assoc :pos (if flip
                                                                        {:a (- (:a pos) (:a n)) :b (- (:b pos) (:b n))}
                                                                        {:a (+ (:a pos) (:a n)) :b (+ (:b pos) (:b n))})))))))
                                              {:pos {:a 0 :b 0} :h 0 :children []}
                                              rule))]
                        (when-not (= 4 (count children))
                          (throw (ex-info (str "lsystem: rule \"" rule "\" must have 4 children") {:rule rule})))
                        children))
        children-of (merge (into {} (map (fn [m] [m (child-table (get rules m))]) motifs))
                           (into {} (map (fn [m] [(Character/toLowerCase m) (child-table (reverse-motif (get rules m)))])
                                         motifs)))
        children (vec (mapcat (fn [m] (get children-of m)) all-motifs))
        ;; ---------- footprint hulls (convex hull of leaf host corners) ----------
        ;; The corner is pre-tripled (the descent works in the corner-sum frame,
        ;; = 3x the (a,b) point frame); the edge direction stays UNIT so the
        ;; containment cross products stay ~O(2^R) instead of O(2^2R): exact
        ;; integer at every resolution. The flipped variant is the hull negated
        ;; (180° = negate, winding-preserving).
        fp-edges (vec (mapcat (fn [m]
                                (let [corners (mapcat (fn [[sym from h]] (turtle/host-corners sym from h))
                                                      (:draws (turtle/walk (to-draws m 1) {:a 0 :b 0} 0)))
                                      hull (convex-hull corners)
                                      n (count hull)]
                                  (for [flip [0 1]
                                        :let [sign (if (pos? flip) -1 1)]]
                                    (vec (mapcat (fn [i]
                                                   (let [c0 (nth hull i)
                                                         c1 (nth hull (mod (inc i) n))]
                                                     [(* (* 3 sign) (:a c0))
                                                      (* (* 3 sign) (:b c0))
                                                      (* sign (- (:a c1) (:a c0)))
                                                      (* sign (- (:b c1) (:b c0)))]))
                                                 (range n))))))
                              all-motifs))
        ;; ---------- leaf tables: per (motif, flip = heading 0|3) the 4 level-1 host cells ----------
        ;;  - corner sums relative to the descent position: sum = 3*pos + leafSum[..]
        ;;  - the cell's pentagon flavor
        leaves (vec (for [m all-motifs
                          flip [0 1]
                          [sym from hh] (:draws (turtle/walk (to-draws m 1) {:a 0 :b 0} (if (pos? flip) 3 0)))]
                      (let [sum (turtle/host-sum sym from hh)
                            upper (Character/toUpperCase sym)]
                        (when-not (contains? flavor-base upper)
                          (throw (ex-info (str "lsystem: no pentagon flavor for draw symbol " sym) {:sym sym})))
                        {:sum sum
                         :flavor (bit-xor (bit-xor (get flavor-base upper) (if (= sym upper) 0 1))
                                          (bit-and hh 1))})))
        tables {:motif-idx motif-idx
                :child-token (mapv #(get motif-idx (:token %)) children)
                :child-flip (mapv #(if (:flip %) 1 0) children)
                :child-off-a (mapv #(:a (:off-unit %)) children)
                :child-off-b (mapv #(:b (:off-unit %)) children)
                :fp-edges fp-edges
                :leaf-sum (vec (mapcat (fn [{{:keys [a b]} :sum}] [a b]) leaves))
                :leaf-flavor (mapv :flavor leaves)}
        ;; ---------- branchless child classifier (3 line tests + LUT per state) ----------
        classifiers (vec (for [m (range motif-count)
                               f [0 1]]
                           (let [tree (build-bsp (child-polys tables m f))
                                 seps (collect-seps tree [])]
                             {:sep (vec (take 9 (concat (apply concat seps) (repeat 0))))
                              :lut (mapv (fn [p] (walk-bsp tree p seps)) (range 8))})))]
    (let [tables (assoc tables
                        :class-sep (vec (mapcat :sep classifiers))
                        :class-lut (vec (mapcat :lut classifiers)))]
      ;; The same tables as primitive arrays, for the descents' inner loops
      (assoc tables :arrays (into {} (map (fn [k] [k (long-array (get tables k))]))
                                  [:child-token :child-flip :child-off-a :child-off-b
                                   :leaf-sum :leaf-flavor :class-sep :class-lut])))))

;; powers of 2 / 4 used by the descents (index by level / digit position)
(def pow2 (mapv #(bit-shift-left 1 %) (range 32)))
(def pow4 (mapv #(bit-shift-left 1 (* 2 %)) (range 20)))
