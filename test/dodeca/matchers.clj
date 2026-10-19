(ns dodeca.matchers
  "vitest's toBeCloseTo and A5's toBeCloseToArray (tests/utils/matchers.ts),
  so ported tests keep A5's tolerances.")

(defn close-to?
  "toBeCloseTo(expected, digits): equal, or within 10^-digits / 2."
  ([actual expected] (close-to? actual expected 2))
  ([actual expected digits]
   (or (== actual expected)
       (< (Math/abs (- (double actual) (double expected)))
          (/ (Math/pow 10 (- digits)) 2)))))

(def default-tolerance 13)

(defn close-to-array?
  "toBeCloseToArray(expected, tolerance): same length, each within 10^-tolerance."
  ([received expected] (close-to-array? received expected default-tolerance))
  ([received expected tolerance]
   (and (= (count received) (count expected))
        (every? true? (map (fn [r e] (< (Math/abs (- (double r) (double e))) (Math/pow 10 (- tolerance))))
                           received expected)))))
