;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.cell
  "Points to cells and cells to points and boundaries.

  A5 keeps a single-entry cache of the most recent lookup, which speeds up
  dense-sample workloads where consecutive calls land in the same cell; it is
  an atom here. A5's lastProjection and lastCellShape, which only its polygon
  fill reads, are not ported."
  (:require [dodeca.core.constants :refer [pi-over-5]]
            [dodeca.core.coordinate-transforms :refer [face->ij from-lon-lat to-lon-lat to-polar normalize-longitudes]]
            [dodeca.core.compaction-marker :refer [compaction-marker?]]
            [dodeca.core.origin :as origin :refer [origins]]
            [dodeca.core.serialization :as s :refer [first-curve-resolution max-resolution res30-quintants world-cell]]
            [dodeca.core.tiling :as tiling]
            [dodeca.geometry.pentagon :as pentagon]
            [dodeca.lattice.index :refer [round-to-triple s->cell triple-flavor triple-in-bounds? triple->s]]
            [dodeca.math.mat2 :as mat2]
            [dodeca.math.vec2 :as vec2]
            [dodeca.projections.dodecahedron :as dodecahedron]
            [dodeca.traversal.neighbors :refer [neighbor-deltas]]
            [dodeca.utils.unsigned :refer [u<]]))

;; The most recent successful lookup, with its pentagon and origin, so the
;; hit-test is one projection + one pentagon containment check.
(def ^:private last-result (atom nil))

(def ^:private face-inradius-haversine
  "Haversine of the angle from a face center to its edge midpoints, atan(2)/2:
  a point closer than that to a face center is on that face."
  0.07467459582398002)

(defn- lookup-in-quintant
  "The best cell for `dodec-point` (face frame of `origin`) within one
  quintant: round to the leaf triangle, closed-form flavor, geometric margin,
  and, when the triangle's cell doesn't strictly contain the point, the best of
  its fixed neighbor deltas. margin > 0 iff the unique strictly-containing
  pentagon. nil when the triple has no curve position."
  [dodec-point origin quintant resolution]
  ;; Inlined quintant->segment (see dodeca.core.origin)
  (let [layout (:orientation origin)
        step (origin/face-step origin)
        delta (mod (+ (- quintant (:first-quintant origin)) 5) 5)
        face-relative-quintant (mod (+ (* step delta) 5) 5)
        orientation (nth layout face-relative-quintant)
        segment (mod (+ (:first-quintant origin) face-relative-quintant) 5)
        ;; Res-30 ids can only encode quintants 0-41: in the others, answer at
        ;; the finest representable resolution instead, the res-29 cell
        ;; CONTAINING the point.
        resolution (if (and (= resolution max-resolution)
                            (>= (+ (* 5 (:id origin)) (mod (+ (- segment (:first-quintant origin)) 5) 5))
                                res30-quintants))
                     (dec max-resolution)
                     resolution)
        [sx sy] (if (zero? quintant)
                  dodec-point
                  (vec2/transform-mat2 dodec-point (mat2/from-rotation (* (* -2 pi-over-5) quintant))))
        curve-level (- (+ 1 resolution) first-curve-resolution)
        scale (Math/pow 2 curve-level)
        px (* sx scale)
        py (* sy scale)
        base (round-to-triple (face->ij [px py]) curve-level)
        max-row (dec (bit-shift-left 1 curve-level))
        base-flavor (triple-flavor base max-row)
        base-margin (tiling/cell-margin-scaled px py (:x base) (:y base) base-flavor)
        ;; All deltas are relative to the ROUNDED triple (the containing
        ;; pentagon is always among its fixed neighbors), not to intermediate best cells.
        [triple flavor margin]
        (if (> base-margin 0)
          [base base-flavor base-margin]
          (reduce (fn [[_ _ margin :as best] d]
                    (let [neighbor {:x (+ (:x base) (:x d)) :y (+ (:y base) (:y d)) :z (+ (:z base) (:z d))}]
                      (if-not (triple-in-bounds? neighbor max-row)
                        best
                        (let [neighbor-flavor (triple-flavor neighbor max-row)
                              neighbor-margin (tiling/cell-margin-scaled px py (:x neighbor) (:y neighbor) neighbor-flavor)]
                          (if (> neighbor-margin margin)
                            (cond-> [neighbor neighbor-flavor neighbor-margin]
                              (> neighbor-margin 0) reduced)
                            best)))))
                  [base base-flavor base-margin]
                  (get-in neighbor-deltas [base-flavor :all])))
        S (triple->s triple curve-level orientation)]
    (when (some? S)
      {:margin margin
       :cell-id (s/serialize {:s S :segment segment :origin origin :resolution resolution})
       :triple triple
       :flavor flavor
       :quintant quintant
       :curve-level curve-level
       :origin-id (:id origin)
       :resolution resolution})))

(defn- accept-candidate
  "Cache the winning pentagon for the dense-sample fast accept and return its id."
  [{:keys [cell-id curve-level quintant triple flavor origin-id resolution]}]
  ;; The pentagon is built when the cache is next consulted, which most
  ;; lookups of scattered points never get to
  (reset! last-result {:cell-id cell-id
                       :pentagon (delay (tiling/get-pentagon-vertices curve-level quintant triple flavor))
                       :origin-id origin-id
                       :resolution resolution})
  cell-id)

;; Tie margin tolerance: containment margins are cross products of unit-scale
;; pentagon edges against coordinates of magnitude up to 2^curveLevel, so their
;; float noise is ~2^(curveLevel - 52); 2^-44 gives a wide safety factor while
;; staying geometrically negligible (cells are unit-size in the scaled frame).
(def ^:private tie-eps (Math/pow 2 -44))

(defn- spherical->cell-boundary
  "Boundary resolution: the point has no strictly-containing pentagon in its
  assigned frame: it lies on a cell edge, or within float noise of a quintant
  or face seam (where the containing cell belongs to a neighboring frame).
  Rerun the same lookup in every frame that could own the point: all 5
  quintants of the 3 nearest faces. A strictly-containing pentagon is unique,
  so the first strict hit wins; if none exists the point is exactly on a
  boundary shared by the near-best candidates, and the tie-break is the cell
  that comes FIRST ALONG THE CURVE: the lowest cell id."
  [spherical resolution first-origin first-quintant first]
  (let [result (reduce
                (fn [candidates origin]
                  (let [dodec-point (dodecahedron/forward spherical (:id origin))
                        ;; This origin's assigned quintant first, then its
                        ;; gamma-adjacent neighbors: seam points resolve in the
                        ;; adjacent frame, so this finds the strict container
                        ;; in 1-2 lookups instead of scanning all 5.
                        q0 (tiling/get-quintant-polar (to-polar dodec-point))]
                    (reduce (fn [candidates dq]
                              (let [quintant (mod (+ q0 dq) 5)]
                                (if (and (= (:id origin) (:id first-origin)) (= quintant first-quintant))
                                  candidates
                                  (let [c (lookup-in-quintant dodec-point origin quintant resolution)]
                                    (cond
                                      (nil? c) candidates
                                      (> (:margin c) 0) (reduced (reduced {:strict c}))
                                      :else (conj candidates c))))))
                            candidates
                            [0 1 4 2 3])))
                (if (some? first) [first] [])
                (origin/find-nearest-origins spherical 3))]
    (if-let [strict (:strict result)]
      (accept-candidate strict)
      (let [best (reduce max ##-Inf (map :margin result))
            eps (* tie-eps (Math/pow 2 (- (+ 1 resolution) first-curve-resolution)))
            winner (reduce (fn [winner c]
                             (if (and (>= (:margin c) (- best eps))
                                      (or (nil? winner) (u< (:cell-id c) (:cell-id winner))))
                               c
                               winner))
                           nil
                           result)]
        (when (nil? winner)
          (throw (ex-info "sphericalToCell: no candidate cell found" {:spherical spherical})))
        (accept-candidate winner)))))

(defn spherical->cell
  "Like `lon-lat->cell`, but for a point already in A5's internal spherical
  representation (rotated authalic frame, as `from-lon-lat` produces it)."
  [spherical resolution]
  (cond
    ;; Resolution -1 represents world-cell, which covers the entire world
    (= resolution -1) world-cell

    (zero? resolution)
    ;; The dodecahedron face containing the point is exact
    (let [origin (origin/find-nearest-origin spherical)
          dodec-point (dodecahedron/forward spherical (:id origin))
          quintant (tiling/get-quintant-polar (to-polar dodec-point))
          {:keys [segment]} (origin/quintant->segment quintant origin)]
      (s/serialize {:s 0 :segment segment :origin origin :resolution resolution}))

    :else
    ;; Try the cached pentagon first: skips the full lookup when consecutive
    ;; calls land in the same cell.
    (let [cached @last-result
          ;; The projection maps points from other faces into this face's frame
          ;; too (the antipodal face center onto this one's center), so the
          ;; cached pentagon only answers for points on its own face.
          nearest-origin (when (and cached (= (:resolution cached) resolution)
                                    (>= (origin/haversine spherical (:axis (nth origins (:origin-id cached))))
                                        face-inradius-haversine))
                           (origin/find-nearest-origin spherical))
          hit (when (and cached (= (:resolution cached) resolution)
                         (or (nil? nearest-origin) (= (:id nearest-origin) (:origin-id cached))))
                (let [projected (dodecahedron/forward spherical (:origin-id cached))]
                  (when (> (pentagon/contains-point @(:pentagon cached) projected) 0)
                    (:cell-id cached))))]
      (or hit
          ;; Fast path: locate the containing pentagon directly. Round to the
          ;; leaf triangle, get the closed-form flavor, and test the pentagon
          ;; geometrically in the scaled quintant frame; the containing pentagon
          ;; is always the triangle's cell or one of its fixed neighbor deltas,
          ;; so at most one 7-candidate walk resolves it, then one curve encode.
          (let [origin (or nearest-origin (origin/find-nearest-origin spherical))
                dodec-point (dodecahedron/forward spherical (:id origin))
                quintant (tiling/get-quintant-polar (to-polar dodec-point))
                best (lookup-in-quintant dodec-point origin quintant resolution)]
            (if (and (some? best) (> (:margin best) 0))
              (accept-candidate best)
              ;; No strictly-containing pentagon in the assigned frame: the point
              ;; sits on a cell boundary or within float noise of a quintant/face seam.
              (spherical->cell-boundary spherical resolution origin quintant best)))))))

(defn lon-lat->cell
  "The cell at `resolution` containing the point [longitude latitude], in degrees."
  [lon-lat resolution]
  (spherical->cell (from-lon-lat lon-lat) resolution))

(defn get-pentagon
  "The pentagon of an A5 cell, in its face's coordinates (A5's _getPentagon)."
  [{:keys [s segment origin resolution]}]
  (let [{:keys [quintant orientation]} (origin/segment->quintant segment origin)]
    (if (= resolution (- first-curve-resolution 2))
      (tiling/get-face-vertices)
      (let [curve-level (+ (- resolution first-curve-resolution) 1)
            {:keys [triple flavor]} (s->cell s curve-level orientation)]
        (tiling/get-pentagon-vertices curve-level quintant triple flavor)))))

(defn cell->spherical [cell]
  (let [{:keys [s segment origin resolution] :as a5cell} (s/deserialize cell)]
    (if (>= resolution (dec first-curve-resolution))
      ;; Fast path: the pentagon center is O(1) from (triple, flavor), no need
      ;; to construct the pentagon itself.
      (let [{:keys [quintant orientation]} (origin/segment->quintant segment origin)
            curve-level (+ (- resolution first-curve-resolution) 1)
            {:keys [triple flavor]} (s->cell s curve-level orientation)]
        (dodecahedron/inverse (tiling/get-pentagon-center curve-level quintant triple flavor) (:id origin)))
      (dodecahedron/inverse (pentagon/get-center (get-pentagon a5cell)) (:id origin)))))

(defn cell-center->cell
  "The cell at `resolution` containing the center of `cell`:
  `(spherical->cell (cell->spherical cell) resolution)` without the round trip
  through the sphere. A cell's center lies on its own face, so the lookup runs
  in that face's frame. A center within float noise of a cell edge, where the
  round trip's own noise decides, goes round the sphere so ties resolve exactly
  as `spherical->cell` resolves them."
  [cell resolution]
  (let [{:keys [s segment origin] cell-resolution :resolution} (s/deserialize cell)
        best (when (and (>= resolution (dec first-curve-resolution))
                        (>= cell-resolution (dec first-curve-resolution)))
               (let [{:keys [quintant orientation]} (origin/segment->quintant segment origin)
                     curve-level (+ (- cell-resolution first-curve-resolution) 1)
                     {:keys [triple flavor]} (s->cell s curve-level orientation)
                     center (tiling/get-pentagon-center curve-level quintant triple flavor)]
                 (lookup-in-quintant center origin (tiling/get-quintant-polar (to-polar center)) resolution)))]
    (if (and (some? best) (> (:margin best) (* tie-eps (Math/pow 2 (:curve-level best)))))
      (:cell-id best)
      (spherical->cell (cell->spherical cell) resolution))))

(defn cell->lon-lat
  "The center of a cell as [longitude latitude], in degrees. The world cell
  gives [0 0]."
  [cell]
  (if (= cell world-cell)
    [0 0]
    (to-lon-lat (cell->spherical cell))))

(defn cell->boundary
  "The boundary of a cell as [longitude latitude] points, counter-clockwise.

  Options: :closed-ring (default true) repeats the first point at the end;
  :segments (default :auto, 2^(6 - resolution) and at least 1) splits each edge
  into that many pieces before projecting, which is what keeps the cells
  equal-area. The world cell and a compaction marker have no boundary."
  ([cell-id] (cell->boundary cell-id {}))
  ([cell-id {:keys [closed-ring segments] :or {closed-ring true segments :auto}}]
   (if (or (= cell-id world-cell) (compaction-marker? cell-id))
     []
     (let [{:keys [origin resolution] :as a5cell} (s/deserialize cell-id)
           segments (if (= segments :auto) (max 1 (long (Math/pow 2 (- 6 resolution)))) segments)
           ;; Split each edge into segments before projection: important to do
           ;; before projection to obtain equal area cells
           vertices (pentagon/split-edges (get-pentagon a5cell) segments)
           boundary (normalize-longitudes
                     (mapv (fn [v] (to-lon-lat (dodecahedron/inverse v (:id origin)))) vertices))
           boundary (if closed-ring (conj boundary (first boundary)) boundary)]
       ;; A5 reverses the pentagon's winding to make the boundary CCW
       (vec (rseq boundary))))))

(defn a5cell-contains-point
  "Positive when the point (spherical) is inside the A5 cell, negative outside."
  [cell spherical]
  (pentagon/contains-point (get-pentagon cell) (dodecahedron/forward spherical (:id (:origin cell)))))

(defn cell-intersects-segment?
  "Whether the segment between two [longitude latitude] points intersects a
  cell. The test runs in the cell's face coordinates: both endpoints are
  projected by the dodecahedron projection and checked against the
  pentagon's straight edges, which is accurate when the segment is short
  relative to the face."
  [cell-id a b]
  (if (= cell-id world-cell)
    true
    (let [cell (s/deserialize cell-id)
          origin-id (:id (:origin cell))]
      (pentagon/intersects-segment? (get-pentagon cell)
                                    (dodecahedron/forward (from-lon-lat a) origin-id)
                                    (dodecahedron/forward (from-lon-lat b) origin-id)))))
