;; dodeca, a Clojure port of A5 (https://github.com/felixpalmer/a5)
;; SPDX-License-Identifier: Apache-2.0
;; Copyright (c) A5 contributors

(ns dodeca.math.hypot
  "ECMAScript's Math.hypot, as V8 computes it. A5 calls it with two and three
  arguments; Jolt's Math/hypot takes two. V8 scales by the largest magnitude
  and sums the squares with Kahan compensation (builtins/math.tq), so this does
  the same, in the same order, unrolled for two and three arguments.")

(defn- kahan
  "One step of the compensated sum: [sum compensation] after adding n²."
  [^double sum ^double compensation ^double n]
  (let [summand (- (* n n) compensation)
        preliminary (+ sum summand)]
    [preliminary (- (- preliminary sum) summand)]))

(defn- special
  "Infinity if any magnitude is infinite, else NaN if any is NaN, else nil."
  [as]
  (cond
    (some #(Double/isInfinite %) as) ##Inf
    (some #(Double/isNaN %) as) ##NaN))

(defn hypot
  (^double [x y]
   (let [ax (Math/abs (double x))
         ay (Math/abs (double y))]
     (if (or (Double/isInfinite ax) (Double/isInfinite ay) (Double/isNaN ax) (Double/isNaN ay))
       (special [ax ay])
       (let [m (max ax ay)]
         (if (zero? m)
           0.0
           (let [nx (/ ax m)
                 ny (/ ay m)
                 s1 (* nx nx)
                 c1 (- (- s1 0.0) s1)
                 summand (- (* ny ny) c1)
                 s2 (+ s1 summand)]
             (* (Math/sqrt s2) m)))))))
  (^double [x y z]
   (let [ax (Math/abs (double x))
         ay (Math/abs (double y))
         az (Math/abs (double z))]
     (if (or (Double/isInfinite ax) (Double/isInfinite ay) (Double/isInfinite az)
             (Double/isNaN ax) (Double/isNaN ay) (Double/isNaN az))
       (special [ax ay az])
       (let [m (max (max ax ay) az)]
         (if (zero? m)
           0.0
           (let [nx (/ ax m)
                 ny (/ ay m)
                 nz (/ az m)
                 s1 (* nx nx)
                 c1 (- (- s1 0.0) s1)
                 summand2 (- (* ny ny) c1)
                 s2 (+ s1 summand2)
                 c2 (- (- s2 s1) summand2)
                 summand3 (- (* nz nz) c2)
                 s3 (+ s2 summand3)]
             (* (Math/sqrt s3) m)))))))
  ([x y z & more]
   (let [as (mapv #(Math/abs (double %)) (list* x y z more))
         m (reduce max 0.0 as)]
     (or (special as)
         (if (zero? m)
           0.0
           (let [[sum] (reduce (fn [[sum c] a] (kahan sum c (/ a m))) [0.0 0.0] as)]
             (* (Math/sqrt sum) m)))))))
