(ns clojure-scraps.strategy
  (:require [clojure.tools.logging :as log]
            [clj-uuid :as uuid]
            [clojure-scraps.aws :as aws-helper]
            [clojure-scraps.datagetter :as datagetter])
  (:import (java.time Duration ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)
           [org.ta4j.core BaseStrategy BaseBarSeries BaseBarSeriesBuilder]
           [org.ta4j.core.criteria pnl.AverageProfitCriterion LinearTransactionCostCriterion MaximumDrawdownCriterion NumberOfBarsCriterion]
           org.ta4j.core.indicators.helpers.ClosePriceIndicator
           org.ta4j.core.indicators.RSIIndicator
           [org.ta4j.core.rules CrossedDownIndicatorRule CrossedUpIndicatorRule WaitForRule]))

(def table-name "strategy-v1")
(def table-key "strategyId")
(defn datetime-parser
  "Parses given datetime string in format yyyy-MM-dd HH:mm:ss."
  [dt]
  (ZonedDateTime/parse dt (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss") (ZoneId/of "UTC"))))

(defn get-data
  "Gets data for one step for dummy strategy signal generation"
  [size]
  (reverse (datagetter/get-time-series size)))

(defn series
  "Bars should be a sequence of maps containing :datetime/:open/:high/:low/:close/:volume"
  ([] (series (get-data 100)))
  ([bars] (let [s (.build (BaseBarSeriesBuilder.))]
            (doseq [{:keys [datetime open high low close volume]} bars]
              (.addBar s (datetime-parser datetime) open high low close volume))
            s)))

(defn rsi-indicator
  "Returns an RSI indicator with given bars"
  [bars period]
  (RSIIndicator. (ClosePriceIndicator. bars) period))

(defn rsi-strat
  "Generates a strategy based on RSI indicator"
  [series period oversold-thresh overbought-thresh]
  (let [rsi (rsi-indicator series period)
        entry (CrossedDownIndicatorRule. rsi oversold-thresh)
        exit (CrossedUpIndicatorRule. rsi overbought-thresh)]
    (BaseStrategy. entry exit)))

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