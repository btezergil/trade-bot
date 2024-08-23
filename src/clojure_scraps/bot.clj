(ns clojure-scraps.bot
  (:require [telegrambot-lib.core :as tbot]
            [environ.core :refer [env]]
            [clojure.tools.logging :as log]
            [clojure-scraps.aws :as aws-helper]
            [clojure.core.match :refer [match]]))

(def trade-bot (tbot/create (env :bot-id)))
(def btezergil-chat-id (env :btezergil-telegram-chat-id))

(defn message-to-me
  "Send a message to me from Telegram"
  [message]
  (log/info "sending message to btezergil")
  (tbot/send-message trade-bot btezergil-chat-id message))

(def config
  {:timeout 10 :sleep 10000}) ;the bot api timeout is in seconds

(defn poll-updates
  "Long poll for recent chat messages from Telegram."
  ([bot]
   (poll-updates bot nil))

  ([bot offset]
   (let [resp (tbot/get-updates bot {:offset offset
                                     :timeout (:timeout config)})]
     (if (:error resp)
       (log/error "tbot/get-updates error:" (:error resp))
       resp))))

(defonce update-id (atom nil))

(defn set-id!
  "Sets the update id to process next as the passed in `id`."
  [id]
  (reset! update-id id))

(defn execute-bot-actions
  "Executes the associated action with Telegram bot commands"
  [bot-command]
  (log/info (format "executing action of bot command: %s " bot-command))

  (condp = bot-command
         "/mokoko" (log/info "come on, mokoko?")
         "/sns" (aws-helper/send-sns "bot wants an SNS sent")))

(defn start-bot
  "Retrieve and process chat messages."
  []
  (log/info "bot service started.")

  (loop []
    (log/info "checking for chat updates.")
    (let [updates (poll-updates trade-bot @update-id)
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
        ;;(println (str msg)) ; your fn that decides what to do with each message.

        ;; Increment the next update-id to process.
        (-> msg
            :update_id
            inc
            set-id!))

      ;; Wait a while before checking for updates again.
      (Thread/sleep (:sleep config)))
    (recur)))
