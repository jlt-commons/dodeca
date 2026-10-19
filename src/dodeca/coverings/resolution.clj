;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.coverings.resolution
  (:require [dodeca.core.compaction-marker :refer [compaction-marker-resolution compaction-marker?]]
            [dodeca.core.serialization :as s]
            [dodeca.utils.unsigned :as u :refer [u<]]))

(defn covering-resolution
  "The resolution of a set of cells: the resolution of its compaction marker,
  or of its finest cell when it has none. A covering stands for all its cells
  at this resolution. -1 for an empty set (or the world cell)."
  [cells]
  ;; A covering ends in its compaction marker, which records the resolution
  (let [last-cell (last cells)]
    (if (and (some? last-cell) (compaction-marker? last-cell))
      (compaction-marker-resolution last-cell)
      ;; Otherwise the finest cell: finer cells have smaller resolution tags (the
      ;; lowest set bit), except at res 30, whose tags are recognised separately
      (loop [cells (seq cells)
             finest-tag 0]
        (if-let [[cell & more] cells]
          (let [tag (if (compaction-marker? cell)
                      (let [resolution (compaction-marker-resolution cell)]
                        (if (= resolution s/max-resolution) ::max (nth s/resolution-tags resolution)))
                      (let [tag (u/lowest-set-bit cell)]
                        (if (zero? (bit-and tag s/res30-tag-bits)) tag ::max)))]
            (cond
              (= tag ::max) s/max-resolution
              (and (not (zero? tag)) (or (zero? finest-tag) (u< tag finest-tag))) (recur more tag)
              :else (recur more finest-tag)))
          (if (zero? finest-tag) -1 (s/get-resolution finest-tag)))))))
