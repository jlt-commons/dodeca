;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.projections.gnomonic
  "Gnomonic projection between a face's tangent plane and the sphere. Polar
  coordinates are [rho gamma]: distance from the face centre and the angle
  around it.")

(defn forward
  "Spherical [theta phi] to polar [rho gamma]."
  [[theta phi]]
  [(Math/tan phi) theta])

(defn inverse
  "Polar [rho gamma] to spherical [theta phi]."
  [[rho gamma]]
  [gamma (Math/atan rho)])
