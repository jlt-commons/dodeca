(ns dodeca.fixture
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

(def ^:private root
  (or (System/getProperty "dodeca.fixtures")
      "test/fixtures"))

(defn path [name]
  (str root "/" name))

(defn load-fixture [name]
  (json/read-str (slurp (path name)) :key-fn keyword))
