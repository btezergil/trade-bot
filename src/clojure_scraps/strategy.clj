(ns clojure-scraps.strategy
  (:require [clojure.tools.logging :as log]
            [clj-uuid :as uuid]
            [clojure-scraps.aws :as aws-helper]
            [clojure-scraps.datagetter :as datagetter]
            [clojure.spec.alpha :as s]
            [clojure-scraps.indicators.pinbar :as pinbar])
  (:import [org.ta4j.core BaseStrategy  BarSeriesManager Trade$TradeType]))

(def table-vars { 
  :table-name "strategy-v1"
  :table-key "strategyId"}) 

(defn constructor [pre-str post-str]
  (fn [class-key args]
    (let [kns       (when-let [x (namespace class-key)] (str x "."))
          class-str (str pre-str kns (name class-key) post-str)]
      (clojure.lang.Reflector/invokeConstructor
        (resolve (symbol class-str))
        (to-array args)))))
(defn ind [class-key & args]
  (let [ctor (constructor "org.ta4j.core.indicators." "Indicator")]
    (ctor class-key args)))
(defn candle-ind [class-key & args]
  (let [ctor (constructor "org.ta4j.core.indicators.candles." "Indicator")]
    (ctor class-key args)))
(defn rule [class-key & args]
  (let [ctor (constructor "org.ta4j.core.rules." "Rule")]
    (ctor class-key args)))
(defn crit [class-key & args]
  (let [ctor (constructor "org.ta4j.core.criteria." "Criterion")]
    (ctor class-key args)))
(defn base-strategy [entry-rule exit-rule]
  (BaseStrategy. entry-rule exit-rule))

(defn rsi-indicator
  "Returns an RSI indicator with given bars"
  [bars period]
  (ind :RSI (ind :helpers/ClosePrice bars) period))

(defn rsi-strategy
  "Generates a strategy based on RSI indicator"
  [series period oversold-thresh overbought-thresh]
  (let [rsi (rsi-indicator series period)
        entry (rule :CrossedDownIndicator rsi oversold-thresh)
        exit (rule :CrossedUpIndicator rsi overbought-thresh)]
    (base-strategy entry exit)))

(defn engulfing-indicator
  "Returns an engulfing indicator"
  [bars]
  (candle-ind :BullishEngulfing bars))

(defn engulfing-strategy
  "Generates a strategy based on engulfing candlestick pattern"
  [series]
  (let [engulfing (engulfing-indicator series)
        entry (rule :BooleanIndicator engulfing)
        exit (rule :WaitFor Trade$TradeType/BUY 10)]
    (base-strategy entry exit)))

(defn hammer-strategy
  []
  (let [ind (pinbar/create-hammer-indicator (datagetter/get-bars))
        entry (rule :BooleanIndicator ind)
        exit (rule :WaitFor Trade$TradeType/BUY 10)]
    (base-strategy entry exit)))

(defn get-profit
  "Returns the profit of given position as a double"
  [position]
  (-> position 
      .getProfit 
      .doubleValue))

(defn calculate-result
  "Gets the positions from a trading record and calculates the total profit/loss"
  [trading-record]
  (reduce + (map get-profit trading-record)))

(defn run-strategy
  "Runs the given strategy and returns the generated positions"
  [strategy]
  (let [bsm (BarSeriesManager. (datagetter/get-bars))] 
    (.getPositions (.run bsm strategy))))

(defn run-rsi
  [oversold overbought]
  (let [strategy (rsi-strategy (datagetter/get-bars) 14 oversold overbought)] 
    (run-strategy strategy)))

(defn run-engulfing
  []
  (let [strategy (engulfing-strategy (datagetter/get-bars))]
    (run-strategy strategy)))

(defn eng-criterion
  [strategy]
  (let [criterion (crit :pnl/NetProfit)
        bars (datagetter/get-bars)
        bsm (BarSeriesManager. bars)
        rec (.run bsm strategy)]
    (.calculate criterion bars rec)))

(calculate-result (run-engulfing))
(eng-criterion (engulfing-strategy (datagetter/get-bars)))
(eng-criterion (hammer-strategy))
(*e)
; TODO: hammer ve shooting star icin candle indicator yaz, sonrasinda da bunlari temel alan stratejiler olustur


(defn run-old
  []
  (let [series (datagetter/get-bars)
        rsi    (rsi-indicator series 14)
        strat  (base-strategy (rule :CrossedDownIndicator rsi 30)
                              (rule :WaitFor Trade$TradeType/BUY 5))
        crits  [(crit :pnl/AverageProfit)
                (crit :EnterAndHoldReturn)
                (crit :LinearTransactionCost 5000 0.005)
                (crit :MaximumDrawdown)
                (crit :NumberOfBars)
                (crit :NumberOfPositions)
                (crit :ValueAtRisk 0.5)
                (crit :pnl/NetProfit)
                (crit :pnl/ProfitLoss)]
        mgr    (BarSeriesManager. series)
        rec    (.run mgr strat)]
    (into {}
          (for [crit crits]
            [(-> crit .getClass .getSimpleName)
             (.longValue (.calculate crit series rec))]))))

;; "TODO: write-to-table yapisi nasil olacak, bunlara karar verip implement et"

(defn write-to-table
  "Writes the given indicator map to the table" 
  [indicator]
  (let [entry {"strategyId" {:S (str (uuid/v1))}
               "indicatorName" {:S "RSI"}
               "buyThreshold" {:N (str (.buy-threshold indicator))}
               "sellThreshold" {:N (str (.sell-threshold indicator))}}]
    (aws-helper/write-to-table (:table-name table-vars) entry)))

(defn read-from-table
  "Reads the given id from strategy table and returns it as a map"
  [id]
  (let [item (aws-helper/read-from-table (:table-name table-vars) (:table-key table-vars) id)
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
    {:id id
     :type indicator-name
     :buy-threshold buy-threshold
     :sell-threshold sell-threshold
     }))
