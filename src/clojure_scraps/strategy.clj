(ns clojure-scraps.strategy
  (:require [clojure.tools.logging :as log]
            [clj-uuid :as uuid]
            [clojure-scraps.aws :as aws-helper]))

(defn get-data
  "Gets data for one step for dummy strategy signal generation"
  []
  40)

(defprotocol Indicator
  (init-indicator [indicator])
  (buy? [indicator])
  (sell? [indicator])
  (write-to-table [indicator]))

(deftype RsiStrategy [id buy-threshold sell-threshold]
  Indicator
  (init-indicator [indicator]
    (log/info (format
                "initialized an indicator for TradingStrategy with params %1 and %2"
                (.buy-threshold indicator) (.sell-threshold indicator))))

  (buy? [indicator]
    (< (get-data) (.buy-threshold indicator)))

  (sell? [indicator]
    (> (get-data) (.sell-threshold indicator)))

  (write-to-table [indicator]
    (let [entry {"strategyId" {:S (str (.id indicator))}
                 "indicatorName" {:S "RSI"}
                 "buyThreshold" {:N (str (.buy-threshold indicator))}
                 "sellThreshold" {:N (str (.sell-threshold indicator))}}]
      (aws-helper/write-to-table "strategy-v1" entry))))
