;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.coverings.compact
  "compact/uncompact for A5 DGGS. A compacted set of cells is a covering: its
  cells sorted in curve order, then a compaction marker recording the
  resolution they stand for (see dodeca.core.compaction-marker)."
  (:require [dodeca.core.cell-info :refer [num-children]]
            [dodeca.core.compaction-marker :refer [compaction-marker?]]
            [dodeca.core.serialization :refer [checked-resolution cell->children]]
            [dodeca.coverings.resolution :refer [covering-resolution]]
            [dodeca.coverings.slot-runs :refer [to-covering]]))

(defn uncompact
  "Expand a set of cells to all their cells at its resolution: the resolution
  of its compaction marker, or of its finest cell when it has none.

  If the input is sorted in curve order (as `compact` returns it), the output
  is too: all children of a cell form a contiguous, ordered block on the
  curve.

  Throws if a value is neither an A5 cell ID nor a compaction marker."
  [cells]
  (let [target-resolution (covering-resolution cells)]
    (into []
          (comp (remove compaction-marker?)
                (mapcat (fn [cell]
                          (let [resolution (checked-resolution cell)]
                            (if (= 1 (num-children resolution target-resolution))
                              [cell]
                              (cell->children cell target-resolution))))))
          cells)))

(defn compact
  "Compact a set of cells: replace every complete group of siblings by their
  parent, recursively, and append a compaction marker recording the
  resolution of the input's finest cell, which `uncompact` expands back to.

  Throws if a value is neither an A5 cell ID nor a compaction marker."
  [cells]
  (to-covering cells (covering-resolution cells)))
