(ns clojure-scraps.genetic
  (:require [clojure-scraps.treenode :as node]
            [clojure-scraps.params :as p]
            [clojure.spec.alpha :as s]
            [clojure.reflect :as r]
            [clojure.pprint :as pp]
            [clojure-scraps.strategy :as strat]
            [clojure-scraps.datagetter :as datagetter]))

(defn get-subseries
  [start end]
  (datagetter/get-subseries-from-bar start end))

(defn generate-sequence
  []
  (conj (node/generate-tree)
        (node/generate-tree)))

(defn generate-signals
  "Generates signals on the given index"
  [tree index direction]
  (if (vector? tree)
    (merge (generate-signals (first tree) index direction) (generate-signals (last tree) index direction))
    {(node/index-to-keyword tree) :long})) ; TODO: call strategy methods for the operators instead of this placeholder

(s/def :genetic/indicator keyword?)
(s/def :genetic/overbought int?)
(s/def :genetic/oversold int?)
(s/def :genetic/window int?)
(s/def :genetic/window1 int?)
(s/def :genetic/window2 int?)
(s/def :genetic/rsi (s/keys :req-un [:genetic/indicator :genetic/overbought :genetic/oversold :genetic/window]))
(s/def :genetic/ma (s/keys :req-un [:genetic/indicator :genetic/window]))
(s/def :genetic/double-ma (s/keys :req-un [:genetic/indicator :genetic/window1 :genetic/window2]))

(defn check-rsi-signal
  [node direction index]
  {:pre [(s/valid? :genetic/rsi node)]}
  (let [{:keys [overbought oversold window]} node
        rsi-indicator (strat/rsi-indicator (get-subseries 0 200) window) ; TODO: burada barlari fix verdik, bunu function input veya baska bir sekilde vermemiz lazim
        rsi-value (.doubleValue (.getValue rsi-indicator index))]
    (cond 
      (and (= direction :long) (<= rsi-value oversold)) :long
      (and (= direction :short) (>= rsi-value overbought)) :short
      :else :no-signal)))

(defn check-single-ma-signal
  [direction index indicator]
  (let [data (get-subseries 0 200)] ; TODO: burada barlari fix verdik, bunu function input veya baska bir sekilde vermemiz lazim
    (cond
      (and (= direction :long) (strat/crosses-up? indicator data index)) :long
      (and (= direction :short) (strat/crosses-down? indicator data index)) :short
      :else :no-signal)))

(defn check-single-sma-signal
  [node direction index]
  {:pre [(s/valid? :genetic/ma node)]}
  (let [{:keys [window]} node
        data (get-subseries 0 200) ; TODO: burada barlari fix verdik, bunu function input veya baska bir sekilde vermemiz lazim
        sma-indicator (strat/sma-indicator data window)]
    (check-single-ma-signal direction index sma-indicator)))

(defn check-single-ema-signal
  [node direction index]
  {:pre [(s/valid? :genetic/ma node)]}
  (let [{:keys [window]} node
        data (get-subseries 0 200) ; TODO: burada barlari fix verdik, bunu function input veya baska bir sekilde vermemiz lazim
        ema-indicator (strat/ema-indicator data window)]
    (check-single-ma-signal direction index ema-indicator)))

(defn check-double-ma-signal
  [direction index ind1 ind2]
  (cond
    (and (= direction :long) (strat/indicators-cross-up? ind1 ind2 index)) :long
    (and (= direction :short) (strat/indicators-cross-down? ind1 ind2 index)) :short
    :else :no-signal))

(defn check-double-sma-signal
  [node direction index]
  {:pre [(s/valid? :genetic/double-ma node)]}
  (let [{:keys [window1 window2]} node
        data (get-subseries 0 200) ; TODO: burada barlari fix verdik, bunu function input veya baska bir sekilde vermemiz lazim
        sma-indicator1 (strat/sma-indicator data window1)
        sma-indicator2 (strat/sma-indicator data window2)]
    (check-double-ma-signal direction index sma-indicator1 sma-indicator2)))

(defn check-double-ema-signal
  [node direction index]
  {:pre [(s/valid? :genetic/double-ma node)]}
  (let [{:keys [window1 window2]} node
        data (get-subseries 0 200) ; TODO: burada barlari fix verdik, bunu function input veya baska bir sekilde vermemiz lazim
        ema-indicator1 (strat/ema-indicator data window1)
        ema-indicator2 (strat/ema-indicator data window2)]
    (check-double-ma-signal direction index ema-indicator1 ema-indicator2)))

; TODO: single sma test, single ema test, double sma test, double ema test

(.getBar (get-subseries 2 200) 0)
(vector? (generate-sequence))
(map (partial check-double-sma-signal (node/generate-double-sma 0) :short) (range 200))
(s/explain :genetic/rsi (node/generate-rsi 0))
(generate-signals (node/generate-tree) 0 :long)
