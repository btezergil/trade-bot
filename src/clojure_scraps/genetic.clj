(ns clojure-scraps.genetic
  (:require [clojure-scraps.treenode :as node]
            [clojure-scraps.params :as p]
            [clojure.spec.alpha :as s]
            [clojure.reflect :as r]
            [clojure.pprint :as pp]
            [clojure-scraps.strategy :as strat]
            [clojure-scraps.datagetter :as datagetter])
  (:import [org.ta4j.core.indicators RSIIndicator]))

(defn get-subseries
  [start end]
  (datagetter/get-subseries-from-bar start end))

(defn generate-sequence
  []
  (conj (node/generate-tree)
        (node/generate-tree)))

(defn generate-signals
  "Generates signals on the given data-index"
  [tree data-index direction]
  (if (vector? tree)
    (merge (generate-signals (first tree) data-index direction) (generate-signals (last tree) data-index direction))
    {(node/index-to-keyword tree) :long})) ; TODO: call strategy methods for the operators instead of this placeholder

; oncelikle her strateji icin verdigimiz periyotta tradingRecord cikartabiliyoruz
; bu tradingRecord array'i icerisinde long sinyali gelen yerde entry var mi diye kontrol edebiliriz

; COZUM ONERISI: strategy ile ugrasmayalim, kendimiz rule satisfaction'u kontrol edelim
; bunu yaparsak her noktada gereksiz yere tum stratejiyi hesaplamis olmuyoruz, indikatorun uzerindeki degerler bizim icin yeterli

(s/def :genetic/indicator keyword?)
(s/def :genetic/overbought int?)
(s/def :genetic/oversold int?)
(s/def :genetic/window int?)
(s/def :genetic/rsi (s/keys :req-un [:genetic/indicator :genetic/overbought :genetic/oversold :genetic/window]))

(defn check-rsi-signal
  [node direction data-index]
  {:pre [(s/valid? :genetic/rsi node)]}
  (let [{:keys [overbought oversold window]} node
        rsi-indicator (strat/rsi-indicator (get-subseries 0 200) window)
        rsi-value (.doubleValue (.getValue rsi-indicator data-index))]
    (cond 
      (and (= direction :long) (<= rsi-value oversold)) :long
      (and (= direction :short) (>= rsi-value overbought)) :short
      :else :no-signal)))


(.getBar (get-subseries 2 200) 0)
(vector? (generate-sequence))
(map (partial check-rsi-signal (node/generate-rsi 0) :short) (range 20))
(s/explain :genetic/rsi (node/generate-rsi 0))
(generate-signals (node/generate-tree) 0 :long)
