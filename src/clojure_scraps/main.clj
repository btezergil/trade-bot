(ns clojure-scraps.main
  (:require [clojure-scraps.genetic :as g]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clojure-scraps.bot :as tb]
            [taoensso.telemere :as t]
            [taoensso.telemere.tools-logging :as tl]))

(defn run-evolution
  "Initial runner function, calls the accessor function to start evolution."
  []
  (let [out (java.io.StringWriter.)
        evolution-result (g/start-evolution)]
    (log/info (pp/pprint (map (fn [res] (dissoc res :parents)) evolution-result) out))
    (tb/message-to-me (.toString out))))

(defn test-individual
  "Generates an individual and calculates its fitness for test purposes."
  []
  (let [ind (g/generate-sequence)
        data g/get-bar-series-for-experiments]
    (log/info "Generated individual for test:" ind)
    (log/info "Calculated fitness:" (g/calculate-fitness data ind))))

(defn setup-telemere
  "Telemere-related setup"
  []
  (tl/tools-logging->telemere!)
  (t/streams->telemere!))

(defn main
  "Parses command line arguments and calls the related functions"
  []
  (setup-telemere)
  (let [arg (first *command-line-args*)]
    (condp = arg
      "r" (run-evolution)
      "t" (time (test-individual))
      (log/warn "No cmdline args"))))

;(test-individual)
;(run-evolution)
;*e
(main)
