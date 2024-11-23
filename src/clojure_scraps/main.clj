(ns clojure-scraps.main
  (:require [clojure-scraps.genetic :as g]
            [clojure-scraps.datagetter :as dg]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]
            [clojure-scraps.bot :as tb]
            [telegrambot-lib.core :as tbot]
            [envvar.core :as envvar :refer [env]]
            [taoensso.telemere :as t]
            [taoensso.telemere.tools-logging :as tl]))

(def telegram-bot-config
  {:timeout 100 :sleep 60000})

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

(defn poll-updates
  "Long poll for recent chat messages from Telegram."
  ([bot]
   (poll-updates bot nil))

  ([bot offset]
   (let [resp (tbot/get-updates bot {:offset offset
                                     :timeout (:timeout telegram-bot-config)})]
     (if (:error resp)
       (log/error "tbot/get-updates error:" (:error resp))
       resp))))

(defn execute-bot-actions
  "Executes the associated action with Telegram bot commands"
  [bot-command]
  (log/info (format "executing action of bot command: %s " bot-command))

  (condp = bot-command
    "/mokoko" (log/info "come on, mokoko?")
    "/start-experiment" (future (run-evolution))
    (log/warn "Unknown command: " bot-command)))

(defn start-bot-long-polling
  "Retrieve and process chat messages from given bot and track the updates with the given update-id atom."
  [bot update-id]
  (log/info "bot service started.")

  (loop []
    (log/info "checking for chat updates.")
    (let [updates (poll-updates bot @update-id)
          messages (:result updates)]
      ;; Check all messages, if any, for commands/keywords.
      (doseq [msg messages]
        (let [bot-command? (= "bot_command" (-> msg
                                                :message
                                                :entities
                                                first
                                                :type))
              command (-> msg
                          :message
                          :text)]
          (when bot-command?
            (log/info (format "received a bot command: %s " command))
            (execute-bot-actions command)))
        ;; Increment the next update-id to process.
        (reset! update-id (-> msg
                              :update_id
                              inc)))
      (Thread/sleep (:sleep telegram-bot-config)))
    (recur)))

(defn main
  "Parses command line arguments and calls the related functions"
  []
  (setup-telemere)
  (let [arg (first *command-line-args*)]
    (condp = arg
      "r" (run-evolution)
      "t" (time (test-individual))
      "b" (let [trade-bot (tbot/create (:bot-id @env))
                update-id (atom nil)]
            (start-bot-long-polling trade-bot update-id))
      (log/warn "No cmdline args"))))

;(test-individual)
;(run-evolution)
;*e
(main)
