;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.compaction-marker
  "The compaction marker: the value a covering carries as its last element,
  recording the resolution its cells stand for. It is a value no cell can
  take: quintant 60 (only 0-59 exist), the resolution in bits 55-48, and the
  marker tag 1000000 in bits 6-0. Cell IDs end in a 1 followed by an odd
  number of zeros, or in one of the res-30 patterns ...1, ...100, ...10000,
  never in a 1 followed by 6 zeros. The other bits carry no meaning yet: they
  are written as 0 and ignored when read.

    63-58    57-56   55-48        47-7          6-0
    111100   00      resolution   reserved (0)  1000000"
  (:require [dodeca.core.serialization :refer [quintant-shift max-resolution]]
            [dodeca.utils.unsigned :refer [ushr u< u<=]]))

(def ^:private compaction-marker-prefix (bit-shift-left 60 quintant-shift))
(def ^:private compaction-marker-end (bit-shift-left 61 quintant-shift))
(def ^:private compaction-marker-resolution-shift 48)
(def ^:private compaction-marker-tag 2r1000000)
(def ^:private low-7-bits 2r1111111)

(defn compaction-marker
  "The compaction marker recording `resolution`."
  [resolution]
  (bit-or compaction-marker-prefix
          (bit-shift-left resolution compaction-marker-resolution-shift)
          compaction-marker-tag))

(defn compaction-marker-resolution
  "The resolution a compaction marker records."
  [value]
  (bit-and (ushr value compaction-marker-resolution-shift) 0xff))

(defn compaction-marker?
  "Check whether a value is a compaction marker: the value a covering carries,
  as its last element, to record its resolution. It is not a cell."
  [value]
  (and (u<= compaction-marker-prefix value)
       (u< value compaction-marker-end)
       (= (bit-and value low-7-bits) compaction-marker-tag)
       (<= (compaction-marker-resolution value) max-resolution)))
