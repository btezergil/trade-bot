(ns clojure-scraps.strategy
  (:require [clojure-scraps.datagetter :as dg]
            [clojure-scraps.params :as p]
            [clojure-scraps.indicators.pinbar :as pinbar]
            [clojure.spec.alpha :as s])
  (:import [org.ta4j.core BaseStrategy Trade$TradeType]
           (org.ta4j.core.backtest BarSeriesManager)))

(def table-vars {:table-name "strategy-v1" :table-key "strategyId"})

(s/def :strategy/signal #{:long :short :no-signal})

;; Reflection functions to access ta4j library

(defn constructor
  [pre-str post-str]
  (fn [class-key args]
    (let [kns (when-let [x (namespace class-key)] (str x "."))
          class-str (str pre-str kns (name class-key) post-str)]
      (clojure.lang.Reflector/invokeConstructor (resolve (symbol class-str)) (to-array args)))))
(defn ind [class-key & args] (let [ctor (constructor "org.ta4j.core.indicators." "Indicator")] (ctor class-key args)))
(defn candle-ind [class-key & args] (let [ctor (constructor "org.ta4j.core.indicators.candles." "Indicator")] (ctor class-key args)))
(defn rule [class-key & args] (let [ctor (constructor "org.ta4j.core.rules." "Rule")] (ctor class-key args)))
(defn crit [class-key & args] (let [ctor (constructor "org.ta4j.core.criteria." "Criterion")] (ctor class-key args)))
(defn base-strategy [entry-rule exit-rule] (BaseStrategy. entry-rule exit-rule))

;; Helper functions for signal generation

(defn get-indicator-value
  "Returns the indicator value on the given index."
  [indicator index]
  (-> indicator
      (.getValue index)
      .doubleValue))

(defn crosses-up?
  "Returns whether the given indicator value crosses up the price."
  [indicator bars index]
  (if (> index 0)
    (let [indicator-value-before (get-indicator-value indicator (dec index))
          indicator-value-current (get-indicator-value indicator index)
          bar-close-before (dg/get-bar-value-at-index bars (dec index))
          bar-close-current (dg/get-bar-value-at-index bars index)]
      (and (< indicator-value-before bar-close-before) (> indicator-value-current bar-close-current)))
    false))

(defn crosses-down?
  "Returns whether the given indicator value crosses down the price."
  [indicator bars index]
  (if (> index 0)
    (let [indicator-value-before (get-indicator-value indicator (dec index))
          indicator-value-current (get-indicator-value indicator index)
          bar-close-before (dg/get-bar-value-at-index bars (dec index))
          bar-close-current (dg/get-bar-value-at-index bars index)]
      (and (> indicator-value-before bar-close-before) (< indicator-value-current bar-close-current)))
    false))

(defn indicators-cross-up?
  "Returns whether the given first indicator value crosses up the second one."
  [ind1 ind2 index]
  (if (> index 0)
    (let [ind1-value-before (get-indicator-value ind1 (dec index))
          ind1-value-current (get-indicator-value ind1 index)
          ind2-value-before (get-indicator-value ind2 (dec index))
          ind2-value-current (get-indicator-value ind2 index)]
      (and (< ind1-value-before ind2-value-before) (> ind1-value-current ind2-value-current)))
    false))

(defn indicators-cross-down?
  "Returns whether the given first indicator value crosses down the second one."
  [ind1 ind2 index]
  (if (> index 0)
    (let [ind1-value-before (get-indicator-value ind1 (dec index))
          ind1-value-current (get-indicator-value ind1 index)
          ind2-value-before (get-indicator-value ind2 (dec index))
          ind2-value-current (get-indicator-value ind2 index)]
      (and (> ind1-value-before ind2-value-before) (< ind1-value-current ind2-value-current)))
    false))

;; Indicator generation functions

(defn rsi-indicator "Returns an RSI indicator with given bars" [bars period] (ind :RSI (ind :helpers/ClosePrice bars) period))

(defn sma-indicator "Returns a SMA indicator with given bars" [bars period] (ind :SMA (ind :helpers/ClosePrice bars) period))

(defn ema-indicator "Returns a EMA indicator with given bars" [bars period] (ind :EMA (ind :helpers/ClosePrice bars) period))

(defn fisher-indicator "Returns a Fisher indicator with given bars" [bars period] (ind :Fisher (ind :helpers/MedianPrice bars) period))

(defn cci-indicator "Returns a CCI indicator with given bars" [bars period] (ind :CCI bars period))

(defn stochastic-oscillator-indicator-k "Returns a stochastic oscillator indicator K with given bars" [bars period] (ind :StochasticOscillatorK bars period))

(defn stochastic-oscillator-indicator-d "Returns a stochastic oscillator indicator D with given bars" [stochK] (ind :StochasticOscillatorD stochK))

(defn parabolic-sar-indicator "Returns a parabolic SAR indicator with given bars" [bars] (ind :ParabolicSar bars))

(defn supertrend-indicator "Returns a supertrend indicator with given bars" [bars period multiplier] (ind :supertrend/SuperTrend bars period multiplier))

(defn engulfing-indicator "Returns an engulfing indicator" [bars] (candle-ind :BullishEngulfing bars))

;; Signal generation functions

(defn combine-signal-list
  "Combines the given signal list into one signal.
  Last received signal has precedence."
  [signal-list]
  {:post [(s/valid? :strategy/signal %)]}
  (let [last-long (.lastIndexOf signal-list :long)
        last-short (.lastIndexOf signal-list :short)]
    (cond (> last-long last-short) :long
          (> last-short last-long) :short
          :else :no-signal)))

(defn check-signal-with-window
  [index signal-check-fn]
  {:post [(s/valid? :strategy/signal %)]}
  (let [window-range (dec (:window-range p/params))
        start-index-for-range (if (pos? (- index window-range)) (- index window-range) 0)]
    (combine-signal-list (map signal-check-fn (range start-index-for-range (inc index))))))
; TODO: burada pmap kullanilabilir mi?

(defn check-rsi-signal-raw
  "Generates signal for RSI, signal condition is:
  long: RSI goes below oversold threshold
  short: RSI goes above overbought threshold"
  [node direction data index]
  {:pre [(s/valid? :genetic/rsi node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [{:keys [overbought oversold window]} node
        rsi-indicator (rsi-indicator data window)
        rsi-value (.doubleValue (.getValue rsi-indicator index))]
    (cond (and (= direction :long) (<= rsi-value oversold)) :long
          (and (= direction :short) (>= rsi-value overbought)) :short
          :else :no-signal)))
; TODO: RSI icin simdilik kullandigimizin disinda baska bir sinyal cikarma yontemi kullanabilir miyiz/kullanmali miyiz

(def check-rsi-signal
  (memoize check-rsi-signal-raw))

(defn check-single-ma-signal
  "Generates signal for single moving averages, signal condition is:
  long: indicator goes above average
  short: indicator goes below average"
  [direction indicator data index]
  {:post [(s/valid? :strategy/signal %)]}
  (cond (and (= direction :long) (crosses-up? indicator data index)) :long
        (and (= direction :short) (crosses-down? indicator data index)) :short
        :else :no-signal))

(defn check-single-sma-signal-raw
  [node direction data index]
  {:pre [(s/valid? :genetic/ma node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [{:keys [window]} node
        sma-indicator (sma-indicator data window)]
    (check-single-ma-signal direction sma-indicator data index)))

(def check-single-sma-signal
  (memoize check-single-sma-signal-raw))

(defn check-single-ema-signal-raw
  [node direction data index]
  {:pre [(s/valid? :genetic/ma node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [{:keys [window]} node
        ema-indicator (ema-indicator data window)]
    (check-single-ma-signal direction ema-indicator data index)))

(def check-single-ema-signal
  (memoize check-single-ema-signal-raw))

(defn check-double-ma-signal
  "Generates signal for double moving averages, signal condition is:
  long: short-interval average goes above long-interval average
  short: short-interval average goes below long-interval average"
  [direction index ind1 ind2]
  {:post [(s/valid? :strategy/signal %)]}
  (cond (and (= direction :long) (indicators-cross-up? ind1 ind2 index)) :long
        (and (= direction :short) (indicators-cross-down? ind1 ind2 index)) :short
        :else :no-signal))

(defn check-double-sma-signal-raw
  [node direction data index]
  {:pre [(s/valid? :genetic/double-ma node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [{:keys [window1 window2]} node
        sma-indicator1 (sma-indicator data window1)
        sma-indicator2 (sma-indicator data window2)]
    (check-double-ma-signal direction index sma-indicator1 sma-indicator2)))

(def check-double-sma-signal
  (memoize check-double-sma-signal-raw))

(defn check-double-ema-signal-raw
  [node direction data index]
  {:pre [(s/valid? :genetic/double-ma node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [{:keys [window1 window2]} node
        ema-indicator1 (ema-indicator data window1)
        ema-indicator2 (ema-indicator data window2)]
    (check-double-ma-signal direction index ema-indicator1 ema-indicator2)))

(def check-double-ema-signal
  (memoize check-double-ema-signal-raw))

(defn check-fisher-signal-raw
  "Generates signal for Fisher transform, signal condition is:
  long: Fisher transform value is above the trigger and below -1
  short: Fisher transform value is below the trigger and above 1"
  [node direction data index]
  {:pre [(s/valid? :genetic/fisher node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [{:keys [window]} node
        fisher-indicator (fisher-indicator data window)
        fisher-value (.doubleValue (.getValue fisher-indicator index))
        trigger-value (.doubleValue (.getValue fisher-indicator (dec index)))]
    (cond (and (= direction :long) (>= fisher-value trigger-value) (<= fisher-value -1)) :long
          (and (= direction :short) (<= fisher-value trigger-value) (>= fisher-value 1)) :short
          :else :no-signal)))
; TODO: fisher sinyali icin alternatifler: 
; TODO: 1. fisher, trigger'i asagi/yukari kirarsa
; TODO: 2. fisher ve RSI ayni anda sinyal verirse
; TODO: 3. fisher ve fibonacci retracement, geri cekilmede fib retracement'tan dondugunde fisher'i teyit olarak kullanip islem ac
; TODO: fisher'da threshold'lari hardcoded verdim, bunlari genetic'e parametre olarak verebiliriz

(def check-fisher-signal
  (memoize check-fisher-signal-raw))

(defn check-cci-signal-raw
  "Generates signal for CCI, signal condition is:
  long: CCI goes over the oversold threshold
  short: CCI goes below the overbought threshold"
  [node direction data index]
  {:pre [(s/valid? :genetic/cci node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [{:keys [overbought oversold window]} node
        cci-indicator (cci-indicator data window)
        cci-value (.doubleValue (.getValue cci-indicator index))
        prev-cci-value (.doubleValue (.getValue cci-indicator (dec index)))]
    (cond (and (= direction :long) (> cci-value oversold) (< prev-cci-value oversold)) :long
          (and (= direction :short) (< cci-value overbought) (> prev-cci-value overbought)) :short
          :else :no-signal)))
; TODO: CCI sinyali icin alternatifler:
; TODO: CCI overbought'u yukari kirarsa al, asagi kirarsa sat (long)
; TODO: CCI 0'i yukari kirarsa al, asagi kirarsa sat

(def check-cci-signal
  (memoize check-cci-signal-raw))

(defn check-stoch-signal-raw
  "Generates signal for Stochastic oscillator, signal condition is:
  long: K crosses D up
  short: K crosses D down"
  [node direction data index]
  {:pre [(s/valid? :genetic/stoch node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [{:keys [window]} node
        stoch-k-indicator (stochastic-oscillator-indicator-k data window)
        stoch-d-indicator (stochastic-oscillator-indicator-d stoch-k-indicator)]
    (cond (and (= direction :long) (indicators-cross-up? stoch-k-indicator stoch-d-indicator index)) :long
          (and (= direction :short) (indicators-cross-down? stoch-k-indicator stoch-d-indicator index)) :short
          :else :no-signal)))
; TODO: stochastic oscillator signal generation would be K crosses D up -> long and K crosses D down -> short
; TODO: alternatively, trade overbought and oversold thresholds
; TODO: alternatively, combine the above logic, look for crosses within oversold region for long and overbought region for short signals

(def check-stoch-signal
  (memoize check-stoch-signal-raw))

(defn check-parabolic-sar-signal-raw
  "Generates signal for Parabolic SAR, signal condition is:
  long: Parabolic SAR goes below the price
  short: Parabolic SAR goes above the price"
  [node direction data index]
  {:pre [(s/valid? :genetic/parabolic-sar node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [parabolic-sar-indicator (parabolic-sar-indicator data)]
    (cond (and (= direction :long) (crosses-down? parabolic-sar-indicator data index)) :long
          (and (= direction :short) (crosses-up? parabolic-sar-indicator data index)) :short
          :else :no-signal)))

(def check-parabolic-sar-signal
  (memoize check-parabolic-sar-signal-raw))

(defn check-supertrend-signal-raw
  "Generates signal for Supertrend, signal condition is:
  long: Supertrend goes below the price
  short: Supertrend goes above the price"
  [node direction data index]
  {:pre [(s/valid? :genetic/supertrend node)]
   :post [(s/valid? :strategy/signal %)]}
  (let [{:keys [window multiplier]} node
        supertrend-indicator (supertrend-indicator data window multiplier)]
    (cond (and (= direction :long) (crosses-down? supertrend-indicator data index)) :long
          (and (= direction :short) (crosses-up? supertrend-indicator data index)) :short
          :else :no-signal)))
; INFO: signal generation is the same as SAR

(def check-supertrend-signal
  (memoize check-supertrend-signal-raw))

;; OLD STUFF THAT WILL MOST PROBABLY BE DELETED

; TODO: we are making the calculations for rules ourselves, check whether we can use the ta4j library for this
(defn rsi-strategy
  "Generates a strategy based on RSI indicator"
  [series period oversold-thresh overbought-thresh]
  (let [rsi (rsi-indicator series period)
        entry (rule :CrossedDownIndicator rsi oversold-thresh)
        exit (rule :CrossedUpIndicator rsi overbought-thresh)]
    (base-strategy entry exit)))

(defn engulfing-strategy
  "Generates a strategy based on engulfing candlestick pattern"
  []
  (let [engulfing (engulfing-indicator (dg/get-bars-from-api))
        entry (rule :BooleanIndicator engulfing)
        exit (rule :WaitFor Trade$TradeType/BUY 10)] ; exit rule nasil dusunuyoruz onu kararlastir
    (base-strategy entry exit)))

(defn hammer-strategy
  []
  (let [ind (pinbar/create-hammer-indicator (dg/get-bars-from-api))
        entry (rule :BooleanIndicator ind)
        exit (rule :WaitFor Trade$TradeType/BUY 10)] ; exit rule naisl dusunuyoruz onu kararlastir
    (base-strategy entry exit)))

(defn run-strategy
  "Runs the given strategy and returns the generated positions"
  [strategy]
  (let [bsm (BarSeriesManager. (dg/get-bars-from-api))]
    (.getPositions (.run bsm strategy))))

(defn run-rsi
  [oversold overbought]
  (let [strategy (rsi-strategy (dg/get-bars-from-api) 14 oversold overbought)]
    (run-strategy strategy)))

;(def run-engulfing (run-strategy (engulfing-strategy)))
;(def run-hammer (run-strategy (hammer-strategy)))

; TODO: criterion does not match actual result, check what the problem is
(defn eng-criterion
  "Uses criterion to find profit BUT NOT MATCHING THE ACTUAL RESULT, DON'T USE!!!"
  [strategy]
  (let [criterion (crit :pnl/NetProfit) bars (dg/get-bars-from-api) bsm (BarSeriesManager. bars) rec (.run bsm strategy)] (.calculate criterion bars rec)))

