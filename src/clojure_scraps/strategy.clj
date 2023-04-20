(ns clojure-scraps.strategy
  (:require [clojure.tools.logging :as log]
            [clj-uuid :as uuid]
            [clojure-scraps.aws :as aws-helper]))

(def table-name "strategy-v1")
(def table-key "strategyId")

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
      (aws-helper/write-to-table table-name entry))))

(defn read-from-table
  "Reads the given id from strategy table and returns an instance of its strategy"
  [id]
  (let [item (aws-helper/read-from-table table-name table-key id)
        data-map (:Item item)
        indicator-name (-> data-map
                           :indicatorName
                           :S)
        buy-threshold (-> data-map
                          :buyThreshold
                          :N
                          parse-long)
        sell-threshold (-> data-map
                          :sellThreshold
                          :N
                          parse-long)]
    (cond
      (= indicator-name "RSI") (->RsiStrategy (uuid/v1) buy-threshold sell-threshold))))