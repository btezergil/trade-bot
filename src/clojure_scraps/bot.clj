(ns clojure-scraps.bot
  (:require [telegrambot-lib.core :as tbot]
            [envvar.core :as envvar :refer [env]]
            [clojure.tools.logging :as log]))

(def trade-bot (tbot/create (:bot-id @env)))
(def btezergil-chat-id (:btezergil-telegram-chat-id @env))

(defn message-to-me
  "Send a message to me from Telegram"
  [message]
  (log/info "sending message to btezergil")
  (tbot/send-message trade-bot btezergil-chat-id message))

