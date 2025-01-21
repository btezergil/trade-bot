(ns clojure-scraps.main
  (:require [clojure-scraps.genetic :as g]
            [clojure-scraps.datagetter :as dg]
            [clojure-scraps.bot :as tb]
            [clojure-scraps.algolab :as algolab]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [taoensso.telemere :as t]
            [taoensso.telemere.tools-logging :as tl]
            [clojure.string :as str]))

(defn run-evolution
  "Initial runner function, calls the accessor function to start evolution."
  []
  (try (let [out (java.io.StringWriter.)
             evolution-result (g/start-evolution)]
         (log/info (pp/pprint (map (fn [res] (dissoc res :parents)) evolution-result) out))
         (tb/message-to-me (.toString out)))
       (catch Exception e (do
                            (tb/message-to-me (str "Caught exception: " (.getMessage e)))
                            (throw e)))))

(defn test-individual
  "Generates an individual and calculates its fitness for test purposes."
  []
  (let [ind (g/generate-sequence)
        data (dg/get-bars-for-genetic :train)]
    (log/info "Generated individual for test:" ind)
    (log/info "Calculated fitness:" (g/calculate-fitness data ind))))

(defn setup-telemere
  "Telemere-related setup"
  []
  (tl/tools-logging->telemere!)
  (t/streams->telemere!))

(defn bot-commands-fn
  "Supported bot commands by the application are defined here."
  [cmd]
  (condp str/starts-with? cmd
    "/start-experiment" (future (run-evolution))
    "/login-to-algolab" (algolab/login)
    "/algolab-sms" (algolab/login-sms-code (-> cmd
                                               (str/split #" ")
                                               second))
    (log/warn "Unknown command:" cmd)))

(defn main
  "Parses command line arguments and calls the related functions"
  []
  (setup-telemere)
  (let [arg (first *command-line-args*)]
    (condp = arg
      "r" (run-evolution)
      "t" (time (test-individual))
      "b" (tb/start-bot-long-polling bot-commands-fn))))

;(test-individual)
;(run-evolution)
;*e
(main)
