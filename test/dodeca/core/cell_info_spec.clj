(ns dodeca.core.cell-info-spec
  (:require [writ.spec :refer [spec assume ann law graph refine]]
            [dodeca.core.cell-info :as info]
            [dodeca.core.constants :refer [authalic-area-earth]]))

(spec dodeca.core.cell-info)

(assume Math/sqrt [Double -> Double])

;; Counts are exact integers: a BigInt past 2^63, which writ's 64-bit Int
;; does not cover (60 * 4^29 cells at resolution 30)
(ann num-cells [Int -> Any])
(ann num-children [Int Int -> Any])
(ann cell-area [Int -> Double])
(ann cell-edge-length-avg [Int -> Double])

;; A resolution A5 has: -1 (the world cell) to 30
(defn resolution? [r] (and (<= -1 r) (<= r 30)))
(refine Resolution [r Int] (resolution? r))

(defn positive? [x] (> x 0))
(defn count? [n] (and (integer? n) (>= n 0)))
(refine Count [n Any] (count? n))
(refine Area [a Double] (positive? a))
(refine Length [l Double] (and (positive? l) (< l 1e7)))

;; From a resolution, the numbers that describe its cells
(graph cell-info
  {:states {:resolution Resolution
            :count Count
            :area Area
            :length Length}
   :edges {:resolution {[num-cells] #{:count}
                        [cell-area] #{:area}
                        [cell-edge-length-avg] #{:length}}}})

;; Resolution 0 is the 12 faces, 1 splits each into 5 quintants, and every
;; later resolution splits each cell into 4
(law num-cells-by-aperture
  (forall [r Resolution]
    (=> (and (<= 0 r) (< r 30))
        (= (info/num-cells (inc r))
           (*' (info/num-cells r) (if (zero? r) 5 4))))))

;; A5 counts no cells below resolution 0: the world cell is not one of a
;; resolution's cells
(law no-cells-below-resolution-zero
  (forall [r Int]
    (=> (< r 0) (= 0 (info/num-cells r)))))

;; The descendants of every cell at p cover the cells at c exactly; the
;; world cell, one cell though no resolution's, has them all. c is a real
;; resolution: at -1 num-cells counts nothing while the world cell is its own
;; single descendant
(law num-children-partitions
  (forall [p Resolution c Resolution]
    (=> (and (<= p c) (<= 0 c))
        (= (info/num-cells c) (*' (info/num-children p c) (if (neg? p) 1 (info/num-cells p)))))))

(law num-children-of-a-finer-cell
  (forall [p Resolution c Resolution]
    (and (=> (< c p) (= 0 (info/num-children p c)))
         (= 1 (info/num-children p p)))))

;; Every cell of a resolution has the same area, so together they are the
;; Earth's, to the rounding of one division; the world cell is all of it
(law cell-area-partitions-the-earth
  (forall [r Resolution]
    (if (neg? r)
      (= authalic-area-earth (info/cell-area r))
      (< (Math/abs (- (* (info/cell-area r) (double (info/num-cells r))) authalic-area-earth))
         (* 1e-15 authalic-area-earth)))))

(law finer-cells-are-smaller
  (forall [r Resolution]
    (=> (< r 30)
        (< (info/cell-area (inc r)) (info/cell-area r)))))

;; The edge length of the world cell is taken as that of a face
(law finer-edges-are-shorter
  (forall [r Resolution]
    (=> (and (<= 0 r) (< r 30))
        (< (info/cell-edge-length-avg (inc r)) (info/cell-edge-length-avg r)))))

(law named-counts
  (and (= 12 (info/num-cells 0))
       (= 60 (info/num-cells 1))
       (= 17293822569102704640N (info/num-cells 30))
       (= 5 (info/num-children 0 1))
       (= 60 (info/num-children -1 1))
       (= 4 (info/num-children 2 3))
       (= 1024 (info/num-children 10 15))
       (= (info/cell-edge-length-avg 0) (info/cell-edge-length-avg -1))))
