;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.core.hex
  "A5's hex spelling of a cell ID: lowercase, without leading zeros, as
  BigInt's toString(16) gives it. `dodeca.utils.unsigned/cell->hex` is the
  16-digit form."
  (:require [dodeca.utils.unsigned :as u]))

(defn hex->u64 [hex]
  (u/hex->cell hex))

(defn u64->hex [index]
  (Long/toHexString index))
