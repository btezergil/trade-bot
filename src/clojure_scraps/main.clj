(ns clojure-scraps.main
  (:require [clojure-scraps.genetic :as g]
            [clojure-scraps.datagetter :as dg]
            [clojure-scraps.bot :as tb]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [taoensso.telemere :as t]
            [taoensso.telemere.tools-logging :as tl]))

(defn run-evolution
  "Initial runner function, calls the accessor function to start evolution."
  [filenames]
  (try (let [out (java.io.StringWriter.)
             evolution-result (g/start-evolution filenames)]
         (log/info (pp/pprint (map (fn [res] (dissoc res :parents)) evolution-result) out))
         (tb/message-to-me (.toString out)))
       (catch Exception e (do
                            (tb/message-to-me (str "Caught exception: " (.getMessage e)))
                            (throw e)))))

(defn test-individual
  "Generates an individual and calculates its fitness for test purposes."
  []
  (let [ind (g/generate-sequence)
        data (dg/get-bars-for-genetic dg/forex-filenames-map :train)]
    (log/info "Generated individual for test:" ind)
    (log/info "Calculated fitness:" (g/calculate-fitness data ind))))

(defn setup-telemere
  "Telemere-related setup"
  []
  (tl/tools-logging->telemere!)
  (t/streams->telemere!))

(defn get-botcmd-arg
  "Gets the received command and returns the argument given to the command."
  [cmd]
  (-> cmd
      (str/split #" ")
      second))

(defn bot-commands-fn
  "Supported bot commands by the application are defined here."
  [cmd]
  (let [cmdarg (get-botcmd-arg cmd)]
    (condp (fn [substr s] (str/starts-with? s substr)) cmd
      "/start-experiment" (future (run-evolution dg/evolution-filenames-map))
      "/test" (log/info "received tail message:" cmdarg)
      (log/warn "Unknown command:" cmd))))

(defn -main
  "Parses command line arguments and calls the related functions"
  []
  (setup-telemere)
  (let [arg (first *command-line-args*)]
    (condp = arg
      "r" (run-evolution dg/evolution-filenames-map)
      "t" (time (test-individual))
      "b" (tb/start-bot-long-polling bot-commands-fn)
      (log/warn "No execution mode defined for the given argument:" arg))))

;(test-individual)
;(run-evolution)
;*e
(-main)
