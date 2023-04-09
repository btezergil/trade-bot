(ns clojure-scraps.bot
  (:require [telegrambot-lib.core :as tbot]
            [environ.core :refer [env]]
            [clojure.tools.logging :as log]
            [cognitect.aws.client.api :as aws]))

(def bot-id (env :bot-id))
(def trade-bot (tbot/create bot-id))
(def btezergil-chat-id (env :btezergil-telegram-chat-id))

(def sns (aws/client {:api :sns}))

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
     (if (contains? resp :error)
       (log/error "tbot/get-updates error:" (:error resp))
       resp))))

(defonce update-id (atom nil))

(defn list-sns-actions
  []
  (aws/ops sns))

(defn send-sns
  "Sends a SNS notification"
  [message]
  (log/info "sending a SNS")
  (aws/invoke sns {:op :Publish :request {:Message message :TopicArn "arn:aws:sns:eu-central-1:994976387571:Sns-deneme-topic"}}))

(defn set-id!
  "Sets the update id to process next as the passed in `id`."
  [id]
  (reset! update-id id))

(defn app
  "Retrieve and process chat messages."
  []
  (log/info "bot service started.")

  (loop []
    (log/info "checking for chat updates.")
    (let [updates (poll-updates trade-bot @update-id)
          messages (:result updates)]

      ;; Check all messages, if any, for commands/keywords.
      (doseq [msg messages]
        (let [is-bot-command (= "bot_command" (-> msg
                                                  :message
                                                  :entities
                                                  first
                                                  :type))]
          (when (true? is-bot-command) (log/info (format "received a bot command: %s " (-> msg
                                                                                           :message
                                                                                           :text)))))
        ;;(println (str msg)) ; your fn that decides what to do with each message.

        ;; Increment the next update-id to process.
        (-> msg
            :update_id
            inc
            set-id!))

      ;; Wait a while before checking for updates again.
      (Thread/sleep (:sleep config)))
    (recur)))
