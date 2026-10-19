(ns dodeca.spec-runner
  "Runs every spec under test/ through writ. An argument names the specs to
  run, so a single one can be iterated on:

    jolt -A:writ -M dodeca.utils.unsigned-spec

  With no argument, all of them."
  (:require [writ.spec :as spec]))

(def specs
  '[dodeca.utils.unsigned-spec
    dodeca.core.cell-info-spec])

(defn -main [& args]
  (let [want (if (seq args)
               (map (comp symbol str) args)
               specs)
        reports (mapv (fn [s]
                        (require s)
                        (let [r (spec/check s {:trials 20})]
                          (println (name s) (if (:ok r) "ok" "FAIL"))
                          (when-not (:ok r) (println (:message r)))
                          r))
                      want)]
    (System/exit (if (every? :ok reports) 0 1))))
