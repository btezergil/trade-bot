(ns clojure-scraps.bot
  (:require [telegrambot-lib.core :as tbot]
            [envvar.core :as envvar :refer [env]]
            [clojure.tools.logging :as log]))

(def telegram-bot-config
  {:timeout 100
   :sleep 10000})
(def trade-bot (tbot/create (:bot-id @env)))
(def btezergil-chat-id (:btezergil-telegram-chat-id @env))

(defn message-to-me
  "Send a message to me from Telegram"
  [message]
  (log/info "sending message to btezergil")
  (tbot/send-message trade-bot btezergil-chat-id message))

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

(defn start-bot-long-polling
  "Retrieve and process chat messages from given bot and track the updates with the given update-id atom."
  [bot-command-fn]
  (log/info "bot service started.")

  (let [bot trade-bot
        update-id (atom nil)]
    (loop []
      (log/info "checking for chat updates.")
      (let [updates (poll-updates bot @update-id)
            messages (:result updates)]
      ;; Check all messages, if any, for commands/keywords.
        (doseq [msg messages]
          (let [message (-> msg :message)
                bot-command? (= "bot_command" (-> message
                                                  :entities
                                                  first
                                                  :type))
                text (-> message :text)]
            (log/info (str "received a message: " msg))
            (when bot-command?
          ;; received message is a bot command, call the bot command function to handle it
              (log/info (format "received a bot command: %s " text))
              (bot-command-fn text)))
        ;; Increment the next update-id to process.
          (reset! update-id (-> msg
                                :update_id
                                inc)))
        (Thread/sleep (:sleep telegram-bot-config)))
      (recur))))

