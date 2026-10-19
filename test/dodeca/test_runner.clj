(ns dodeca.test-runner
  (:require [clojure.test :as t]
            [dodeca.smoke-test]
            [dodeca.core.constants-test]
            [dodeca.core.face-adjacency-test]
            [dodeca.core.origin-test]
            [dodeca.geometry.pentagon-test]
            [dodeca.math-test]
            [dodeca.utils.unsigned-test]
            [dodeca.core.serialization-test]
            [dodeca.core.cell-info-test]
            [dodeca.core.compaction-marker-test]
            [dodeca.coverings.compact-test]
            [dodeca.projections.authalic-test]
            [dodeca.projections.gnomonic-test]
            [dodeca.utils.vector-test]
            [dodeca.geometry.spherical-polygon-test]
            [dodeca.math.ecma-test]
            [dodeca.core.tiling-test]
            [dodeca.lattice.curve-test]
            [dodeca.lattice.lsystem-test]
            [dodeca.lattice.compat-test]
            [dodeca.traversal.quintant-neighbors-test]
            [dodeca.hierarchy-test]
            [dodeca.core.coordinate-transforms-test]
            [dodeca.projections.crs-test]
            [dodeca.projections.equal-area-test]
            [dodeca.projections.dodecahedron-test]
            [dodeca.core.cell-test]
            [dodeca.traversal.grid-disk-test]
            [dodeca.core-test]
            [dodeca.crosscheck-test]))

(def namespaces
  '[dodeca.smoke-test
    dodeca.core.constants-test
    dodeca.core.face-adjacency-test
    dodeca.core.origin-test
    dodeca.geometry.pentagon-test
    dodeca.math-test
    dodeca.utils.unsigned-test
    dodeca.core.serialization-test
    dodeca.core.cell-info-test
    dodeca.core.compaction-marker-test
    dodeca.coverings.compact-test
    dodeca.projections.authalic-test
    dodeca.projections.gnomonic-test
    dodeca.utils.vector-test
    dodeca.geometry.spherical-polygon-test
    dodeca.math.ecma-test
    dodeca.core.tiling-test
    dodeca.lattice.curve-test
    dodeca.lattice.lsystem-test
    dodeca.lattice.compat-test
    dodeca.traversal.quintant-neighbors-test
    dodeca.hierarchy-test
    dodeca.core.coordinate-transforms-test
    dodeca.projections.crs-test
    dodeca.projections.equal-area-test
    dodeca.projections.dodecahedron-test
    dodeca.core.cell-test
    dodeca.traversal.grid-disk-test
    dodeca.core-test
    dodeca.crosscheck-test])

(defn -main [& _]
  (let [{:keys [fail error]} (apply t/run-tests namespaces)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
