(ns clojure-scraps.strategy
  (:require [clojure.tools.logging :as log]))

(defn get-data
  "Gets data for one step for dummy strategy signal generation"
  []
  40)

(defprotocol Indicator
  (init-indicator [indicator])
  (buy? [indicator])
  (sell? [indicator]))

(deftype RsiStrategy [buy-threshold sell-threshold]
  Indicator
  (init-indicator [indicator]
    (log/info (format "initialized an indicator for TradingStrategy with params %1 and %2" (.buy-threshold indicator) (.sell-threshold indicator))))
  (buy? [indicator]
    (< (get-data) (.buy-threshold indicator)))
  (sell? [indicator]
    (> (get-data) (.sell-threshold indicator))))
