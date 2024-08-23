(ns clojure-scraps.main
  (:require [clojure-scraps.genetic :as g]))

(defn run-evolution
  "Initial runner function, calls the accessor function to start evolution."
  []
  (g/start-evolution))

(run-evolution)
#_*e
