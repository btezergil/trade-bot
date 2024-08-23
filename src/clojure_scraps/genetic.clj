(ns clojure-scraps.genetic
  (:require [clj-uuid :as uuid]
            [clojure-scraps.datagetter :as datagetter]
            [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.monitors :as mon]
            [clojure-scraps.params :as p]
            [clojure-scraps.strategy :as strat]
            [clojure-scraps.treenode :as node]
            [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [nature.core :as n]
            [nature.initialization-operators :as io]
            [nature.monitors :as nmon]))

(defn get-subseries [start end] (datagetter/get-subseries-from-bar start end))

(defn generate-sequence [] (vector (node/generate-tree) (node/generate-tree)))

(defn check-rsi-signal
  [node direction index data]
  {:pre [(s/valid? :genetic/rsi node)]}
  (let [{:keys [overbought oversold window]} node
        rsi-indicator (strat/rsi-indicator data window)
        rsi-value (.doubleValue (.getValue rsi-indicator index))]
    (cond (and (= direction :long) (<= rsi-value oversold)) :long
          (and (= direction :short) (>= rsi-value overbought)) :short
          :else :no-signal)))

(defn check-single-ma-signal
  [direction index indicator data]
  (cond (and (= direction :long) (strat/crosses-up? indicator data index)) :long
        (and (= direction :short) (strat/crosses-down? indicator data index)) :short
        :else :no-signal))

(defn check-single-sma-signal
  [node direction index data]
  {:pre [(s/valid? :genetic/ma node)]}
  (let [{:keys [window]} node sma-indicator (strat/sma-indicator data window)] (check-single-ma-signal direction index sma-indicator data)))

(defn check-single-ema-signal
  [node direction index data]
  {:pre [(s/valid? :genetic/ma node)]}
  (let [{:keys [window]} node ema-indicator (strat/ema-indicator data window)] (check-single-ma-signal direction index ema-indicator data)))

(defn check-double-ma-signal
  [direction index ind1 ind2]
  (cond (and (= direction :long) (strat/indicators-cross-up? ind1 ind2 index)) :long
        (and (= direction :short) (strat/indicators-cross-down? ind1 ind2 index)) :short
        :else :no-signal))

(defn check-double-sma-signal
  [node direction index data]
  {:pre [(s/valid? :genetic/double-ma node)]}
  (let [{:keys [window1 window2]} node
        sma-indicator1 (strat/sma-indicator data window1)
        sma-indicator2 (strat/sma-indicator data window2)]
    (check-double-ma-signal direction index sma-indicator1 sma-indicator2)))

(defn check-double-ema-signal
  [node direction index data]
  {:pre [(s/valid? :genetic/double-ma node)]}
  (let [{:keys [window1 window2]} node
        ema-indicator1 (strat/ema-indicator data window1)
        ema-indicator2 (strat/ema-indicator data window2)]
    (check-double-ma-signal direction index ema-indicator1 ema-indicator2)))

(defn generate-signals
  "Generates signals on the given data index."
  [tree direction index data]
  (if (vector? tree)
    (merge (generate-signals (first tree) direction index data) (generate-signals (last tree) direction index data))
    (let [index-keyword (node/index-to-keyword tree)]
      (log/debug "Generating signal for " tree)
      (condp = (:indicator tree)
        :rsi {index-keyword (check-rsi-signal tree direction index data)}
        :sma {index-keyword (check-single-sma-signal tree direction index data)}
        :ema {index-keyword (check-single-ema-signal tree direction index data)}
        :double-sma {index-keyword (check-double-sma-signal tree direction index data)}
        :double-ema {index-keyword (check-double-ema-signal tree direction index data)}
        :identity {index-keyword :identity}))))

(defn long? "Checks whether the generated individual signals result in a overall long signal for this strategy." [tree signals] (= :long (node/signal-check tree signals :long)))

(defn short? "Checks whether the generated individual signals result in a overall short signal for this strategy." [tree signals] (= :short (node/signal-check tree signals :short)))

(defn transaction-result "Calculates the profit of a single transaction." [direction initial-price final-price] (if (= :long direction) (- final-price initial-price) (- initial-price final-price)))

(defn scale-profit-result
  "Scales the profit result to a positive value.
  Profit value needs to be positive since it is used for weighted selection."
  [total-profit]
  (if (>= total-profit 0) (+ total-profit 1) (Math/pow 2 total-profit)))

(defn calculate-profit-from-transactions
  "Calculates the total profit of given transactions."
  [transactions final-price]
  (if-not (empty? transactions)
    (let [initial-position (first transactions)
          final-position (second transactions)
          transaction-result-partial (partial transaction-result (:position initial-position) (:price initial-position))]
      (if (nil? final-position) (transaction-result-partial final-price) (+ (transaction-result-partial (:price final-position)) (calculate-profit-from-transactions (rest transactions) final-price))))
    0))

(defn calculate-scaled-profit
  "Calculates the total profit and scales it so that the result is positive."
  [transactions final-price]
  (-> transactions
      (calculate-profit-from-transactions final-price)
      scale-profit-result))

(defn create-transaction-map
  "Creates a transaction to be added to the transaction array
  Records the direction, time, and price"
  [data current-index direction]
  {:price (datagetter/get-bar-value-at-index data current-index), :position direction, :bar-time (datagetter/get-bar-time-at-index data current-index)})

(defn calculate-fitness
  "Calculates the fitness of given genetic sequence."
  [data genetic-sequence]
  (let [max-index (.getBarCount data)]
    (loop [current-position :none
           current-index 0
           transactions (vector)]
      (if (< current-index max-index)
        (let [long-signals (generate-signals (first genetic-sequence) :long current-index data)
              short-signals (generate-signals (last genetic-sequence) :short current-index data)]
          (cond (and (long? (first genetic-sequence) long-signals) (not= current-position :long))
                (recur :long (inc current-index) (conj transactions (create-transaction-map data current-index :long)))
                (and (short? (last genetic-sequence) short-signals) (not= current-position :short))
                (recur :short (inc current-index) (conj transactions (create-transaction-map data current-index :short)))
                :else (recur current-position (inc current-index) transactions)))
        (calculate-scaled-profit transactions (datagetter/get-bar-value-at-index data (dec max-index)))))))

(defn calculate-transactions-for-monitor
  "Calculates the transactions of given genetic sequence for bookkeeping."
  [data individual]
  (let [max-index (.getBarCount data)
        genetic-sequence (:genetic-sequence individual)]
    (loop [current-position :none
           current-index 0
           transactions (vector)]
      (if (< current-index max-index)
        (let [long-signals (generate-signals (first genetic-sequence) :long current-index data)
              short-signals (generate-signals (last genetic-sequence) :short current-index data)]
          (cond (and (long? (first genetic-sequence) long-signals) (not= current-position :long))
                (recur :long (inc current-index) (conj transactions (create-transaction-map data current-index :long)))
                (and (short? (last genetic-sequence) short-signals) (not= current-position :short))
                (recur :short (inc current-index) (conj transactions (create-transaction-map data current-index :short)))
                :else (recur current-position (inc current-index) transactions)))
        transactions))))

; TODO: make data getting part generic and use that instead of calling get-subseries everywhere

(defn start-evolution
  "Starts evolution, this method calls the nature library with the necessary params."
  []
  (let [evolution-id (uuid/v4)]
    (dyn/write-evolution-to-table evolution-id)
    (n/evolve-with-sequence-generator generate-sequence
                                      (:population-size p/params)
                                      (:generation-count p/params)
                                      (partial calculate-fitness (get-subseries 0 300))
                                      [(partial node/crossover (partial calculate-fitness (get-subseries 0 300)))]
                                      [(partial node/mutation (partial calculate-fitness (get-subseries 0 300)))]
                                      {:solutions 3,
                                       :carry-over 1,
                                       :monitors [nmon/print-best-solution mon/print-average-fitness-of-population (partial mon/write-individuals-to-table-monitor evolution-id)
                                                  (partial mon/write-transactions-to-table-monitor (partial calculate-transactions-for-monitor (get-subseries 0 300)))]})))

