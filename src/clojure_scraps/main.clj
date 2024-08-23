(ns clojure-scraps.main
  (:require [clojure-scraps.genetic :as g]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clojure-scraps.bot :as tb]))

(defn run-evolution
  "Initial runner function, calls the accessor function to start evolution."
  []
  (let [out (java.io.StringWriter.)
        evolution-result (g/start-evolution)]
    (pp/pprint (map (fn [res] (dissoc res :parents)) evolution-result) out)
    (tb/message-to-me (.toString out))))

(time (run-evolution))
;*e
