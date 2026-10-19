;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.projections.authalic
  "Geodetic <-> authalic latitude.

  Coefficients from https://arxiv.org/pdf/2212.05818 (see A5's
  authalic_constants.py for their derivation). `apply-coefficients` is an
  adaptation of applyCoefficients from the DGGAL project (authalic.ec),
  Copyright (c) 2014-2025, Ecere Corporation, under the BSD 3-Clause License;
  see NOTICE.")

(def ^:private geodetic-to-authalic
  [-2.2392098386786394e-3 2.1308606513250217e-6 -2.5592576864212742e-9 3.3701965267802837e-12
   -4.6675453126112487e-15 6.6749287038481596e-18])

(def ^:private authalic-to-geodetic
  [2.2392089963541657e-3 2.8831978048607556e-6 5.0862207399726603e-9 1.02018123778161e-11
   2.1912872306767718e-14 4.9284235482523806e-17])

(defn- apply-coefficients
  "Clenshaw summation (order 6) of the coefficients C at phi, in radians."
  [phi C]
  (let [sin-phi (Math/sin phi)
        cos-phi (Math/cos phi)
        X (* (* 2 (- cos-phi sin-phi)) (+ cos-phi sin-phi))
        u0 (+ (* X (nth C 5)) (nth C 4))
        u1 (+ (* X u0) (nth C 3))
        u0 (+ (- (* X u1) u0) (nth C 2))
        u1 (+ (- (* X u0) u1) (nth C 1))
        u0 (+ (- (* X u1) u0) (nth C 0))]
    (+ phi (* (* (* 2 sin-phi) cos-phi) u0))))

(defn forward
  "Geodetic latitude to authalic latitude, in radians."
  [phi]
  (apply-coefficients phi geodetic-to-authalic))

(defn inverse
  "Authalic latitude to geodetic latitude, in radians."
  [phi]
  (apply-coefficients phi authalic-to-geodetic))
