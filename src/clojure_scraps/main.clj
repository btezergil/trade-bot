(ns clojure-scraps.main
  (:require [clojure-scraps.genetic :as g]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]))

(defn run-evolution "Initial runner function, calls the accessor function to start evolution." [] (pp/pprint (g/start-evolution)))

(run-evolution)
;*e
