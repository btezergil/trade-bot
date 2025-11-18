(ns clojure-scraps.main
  (:require [clojure-scraps.genetic :as g]
            [clojure-scraps.datagetter :as dg]
            [clojure-scraps.bot :as tb]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clojure.string :as str]
            [taoensso.telemere :as t]
            [taoensso.telemere.tools-logging :as tl])
  (:gen-class))

(def allowed-params #{:pop-size :gen-cnt :fitness-crit})

(defn run-evolution
  "Initial runner function, calls the accessor function to start evolution."
  ([filenames] (run-evolution filenames nil))
  ([filenames opt-args]
   (try (let [out (java.io.StringWriter.)
              evolution-result (g/start-evolution filenames opt-args)]
          (log/info (pp/pprint (map (fn [res] (dissoc res :parents)) evolution-result) out))
          (tb/message-to-me (.toString out)))
        (catch Exception e (do
                             (tb/message-to-me (str "Caught exception: " (.getMessage e)))
                             (throw e))))))

(defn run-evolution-with-futures
  [filenames opt-args]
  (let [res1 (future (run-evolution filenames opt-args))
        res2 (future (run-evolution filenames opt-args))]
    @res1
    @res2))

(defn test-individual
  "Generates an individual and calculates its fitness for test purposes."
  []
  (let [ind (g/generate-sequence)
        data (dg/get-bars-for-genetic dg/forex-filenames-map :train)]
    (log/info "Generated individual for test:" ind)
    (log/info "Calculated fitness:" (g/calculate-fitness data :profit ind))))

(defn setup-telemere
  "Telemere-related setup"
  []
  (tl/tools-logging->telemere!)
  (t/streams->telemere!)
  (t/check-interop))

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

(defn- parse-arg
  [argmap arg]
  (let [argpair (str/split arg #"=")
        k (keyword (first argpair))
        v (second argpair)]
    (if (contains? allowed-params k)
      (if (= k :fitness-crit)
        (assoc argmap k (keyword v))
        (assoc argmap k (Integer/parseInt v)))
      argmap)))

(defn -main
  "Parses command line arguments and calls the related functions"
  [& args]
  (setup-telemere)
  (log/info "Number of processors in clj: " (.availableProcessors (Runtime/getRuntime)))
  (let [arg (first args)
        opt-args (reduce parse-arg {} (rest args))]
    (log/info "Accepted optional arguments: " opt-args)
    (condp = arg
      "r" (run-evolution-with-futures dg/evolution-filenames-map opt-args)
      "t" (time (test-individual))
      "b" (tb/start-bot-long-polling bot-commands-fn)
      (log/error "No execution mode defined for the given argument:" arg))))

;(-main)
