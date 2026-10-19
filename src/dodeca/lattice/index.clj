;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.lattice.index
  "The canonical A5 curve is the non-self-intersecting L-system curve
  (lsystem + curve): point location via round-to-triple, s <-> cell mappings
  via s->cell / s->triple / triple->s. This is a breaking change from releases
  before A5 1.0: cell IDs differ from the original construction, which remains
  available bit-for-bit via the compat-* functions for migration.

  Orientations are keywords :uv :vu :uw :wu :vw :wv: which corner the curve
  starts and ends at (:wv starts at w and ends at v)."
  (:require [dodeca.lattice.compat :as compat]
            [dodeca.lattice.curve :as curve]
            [dodeca.lattice.lsystem.index :as lsystem]
            [dodeca.lattice.triple :as triple]))

(def round-to-triple curve/round-to-triple)

(def curve-child lsystem/curve-child)
(def s->cell lsystem/s->cell)
(def s->triple lsystem/s->triple)
(def triple->curve-node lsystem/triple->curve-node)

(def triple-parity triple/triple-parity)
(def triple-in-bounds? triple/triple-in-bounds?)
(def triple-flavor triple/triple-flavor)
(def triple->s triple/triple->s)

;; The ORIGINAL (pre-L-system) curve, bit-for-bit, for the migration path:
;; same cells, same pentagon flavors, old visiting order.
(def compat-s->cell compat/compat-s->cell)
(def compat-s->triple compat/compat-s->triple)
(def compat-triple->s compat/compat-triple->s)
(def compat-ij->s compat/compat-ij->s)
