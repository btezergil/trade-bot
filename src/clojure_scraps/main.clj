(ns clojure-scraps.main
  (:require [clojure-scraps.genetic :as g]
            [clojure-scraps.datagetter :as dg]
            [clojure-scraps.bot :as tb]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [btezergil.algolab-lib :as algolab]
            [taoensso.telemere :as t]
            [taoensso.telemere.tools-logging :as tl]))

(def forex-filename  "data/forex/eurusd-3month-1h.csv")
(def forex-test-filename  "data/forex/eurusd-45days-1h-test.csv")
(def forex-filenames-map {:train-file forex-filename :test-file forex-test-filename})
(def bist-stock-filenames (map (fn [filename] (str "data/bist/" filename))
                               (filter (fn [file] (str/ends-with? file ".csv"))
                                       (str/split (:out (sh/sh "ls" "data/bist")) #"\n"))))

(defn get-filename-from-stock
  "Gets the filename that contains CSV data for given stock. Prepend an F to the stock name for VIOP data."
  [stock]
  (first (filter (fn [filename] (str/includes? filename (str "_" stock))) bist-stock-filenames)))
(def bist-filenames-map (let [filename (get-filename-from-stock "SAHOL")]
                          {:train-file filename :test-file filename}))

(def evolution-filenames-map bist-filenames-map)

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
        data (dg/get-bars-for-genetic forex-filenames-map :train)]
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
  (condp (fn [substr s] (str/starts-with? s substr)) cmd
    "/start-experiment" (future (run-evolution evolution-filenames-map))
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
      "r" (run-evolution evolution-filenames-map)
      "t" (time (test-individual))
      "b" (tb/start-bot-long-polling bot-commands-fn))))

;(test-individual)
;(run-evolution)
;*e
(main)
