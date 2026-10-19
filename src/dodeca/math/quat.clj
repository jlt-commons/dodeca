;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.math.quat
  "Double-precision quaternions as [x y z w]. A5 only conjugates, which for a
  unit quaternion is also its inverse.")

(defn conjugate [[qx qy qz qw]]
  [(- qx) (- qy) (- qz) qw])
