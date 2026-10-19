;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core
  "The public API, as A5's modules/index.ts exports it, for the subset dodeca
  ports. Cell IDs are longs holding A5's unsigned 64 bits: ids at or above
  2^63 read negative, so order them with `dodeca.utils.unsigned/unsigned-compare`
  and spell them with `cell->hex`."
  (:require [dodeca.core.cell :as cell]
            [dodeca.core.cell-info :as cell-info]
            [dodeca.core.compaction-marker :as marker]
            [dodeca.core.hex :as hex]
            [dodeca.core.serialization :as serialization]
            [dodeca.coverings.compact :as compact]
            [dodeca.traversal.grid-disk :as grid-disk]))

;; Indexing

(defn lon-lat->cell
  "The cell at `resolution` (-1 to 30) containing [longitude latitude], in degrees."
  [lon-lat resolution]
  (cell/lon-lat->cell lon-lat resolution))

(defn cell->lon-lat
  "The center of a cell as [longitude latitude], in degrees."
  [cell]
  (cell/cell->lon-lat cell))

(defn cell->boundary
  "The boundary of a cell as [longitude latitude] points, counter-clockwise.
  Options: :closed-ring (default true), :segments per edge (default :auto)."
  ([cell] (cell/cell->boundary cell))
  ([cell opts] (cell/cell->boundary cell opts)))

(defn hex->cell
  "The cell ID a hex string spells (A5's hexToU64)."
  [hex]
  (hex/hex->u64 hex))

(defn cell->hex
  "A cell ID as lowercase hex without leading zeros (A5's u64ToHex)."
  [cell]
  (hex/u64->hex cell))

;; Hierarchy

(def max-resolution serialization/max-resolution)

(def world-cell
  "The abstract cell containing the whole world: resolution -1, parent of the
  12 resolution-0 cells."
  serialization/world-cell)

(defn cell->parent
  "The ancestor of a cell at `resolution` (default: one coarser)."
  ([cell] (serialization/cell->parent cell))
  ([cell resolution] (serialization/cell->parent cell resolution)))

(defn cell->children
  "The descendants of a cell at `resolution` (default: one finer), in
  ascending ID order."
  ([cell] (serialization/cell->children cell))
  ([cell resolution] (serialization/cell->children cell resolution)))

(defn resolution
  "The resolution of a cell (A5's getResolution): -1 for the world cell."
  [cell]
  (serialization/get-resolution cell))

(defn res0-cells
  "The 12 resolution-0 cells, the dodecahedron's faces."
  []
  (serialization/get-res0-cells))

(defn valid-cell?
  "Whether a 64-bit value is an A5 cell ID. The world cell is one; a
  compaction marker is not."
  [cell]
  (serialization/valid-cell? cell))

(defn num-cells
  "The number of cells at a resolution: a long, or a BigInt at resolution 30."
  [resolution]
  (cell-info/num-cells resolution))

(defn num-children
  "The number of descendants a cell at `parent-resolution` has at `child-resolution`."
  [parent-resolution child-resolution]
  (cell-info/num-children parent-resolution child-resolution))

(defn cell-area
  "The area of a cell at a resolution, in square metres. Every cell of a
  resolution has the same area."
  [resolution]
  (cell-info/cell-area resolution))

(defn cell-edge-length-avg
  "The average edge length of a cell at a resolution, in metres."
  [resolution]
  (cell-info/cell-edge-length-avg resolution))

;; Compaction

(defn compact
  "The coarsest cells covering a set of cells, in curve order, then a
  compaction marker recording the finest resolution."
  [cells]
  (compact/compact cells))

(defn uncompact
  "Every cell of a covering at its resolution: its compaction marker's, or its
  finest cell's."
  [cells]
  (compact/uncompact cells))

(defn compaction-marker?
  "Whether a value is the compaction marker a covering ends with."
  [value]
  (marker/compaction-marker? value))

;; Traversal

(defn grid-disk
  "The cells within k edge-sharing hops of a cell, center included, as a
  covering: compacted, in curve order, then a compaction marker. `uncompact`
  gives every cell at the cell's resolution."
  [cell k]
  (grid-disk/grid-disk cell k))

(defn grid-disk-vertex
  "As `grid-disk`, with cells sharing only a vertex counting as neighbors too."
  [cell k]
  (grid-disk/grid-disk-vertex cell k))
