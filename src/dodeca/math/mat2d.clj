;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.math.mat2d
  "Double-precision 2x3 affine matrices as [a b c d tx ty], gl-matrix's
  mat2d layout. A5 only builds them from values.")

(defn from-values [a b c d tx ty]
  [a b c d tx ty])
