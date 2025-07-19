(ns clojure-scraps.genetic
  (:require [clojure-scraps.datagetter :as dg]
            [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.monitors :as mon]
            [clojure-scraps.params :as p]
            [clojure-scraps.strategy :as strat]
            [clojure-scraps.evaltree :as tree]
            [clojure-scraps.bot :as tb]
            [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clojure.math :as math]
            [clj-uuid :as uuid]
            [nature.core :as n]
            [envvar.core :as envvar :refer [env]]
            [nature.monitors :as nmon]))

(defn generate-sequence
  "Generates a genetic sequence for individual."
  []
  [(tree/generate-tree) (tree/generate-tree)])

(s/def :transaction/result double?)
(s/def :transaction/position #{:long :short})
(s/def :transaction/time-range string?) ; TODO: buradaki datetime yapisi icin ayri bir spec yazalim
(s/def :genetic/transaction (s/keys :req-un [:transaction/result :transaction/position :transaction/time-range]))

;; Signal generation functions

(defn generate-signals
  "Generates signals on the given data index."
  [tree direction index data]
  (if (vector? tree)
    (merge (generate-signals (first tree) direction index data) (generate-signals (last tree) direction index data))
    (let [index-keyword (tree/index-to-keyword tree)]
      (log/debug "Generating signal for " tree)
      (condp = (:indicator tree)
        :rsi {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-rsi-signal tree direction data index)))}
        :sma {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-single-sma-signal tree direction data index)))}
        :ema {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-single-ema-signal tree direction data index)))}
        :double-sma {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-double-sma-signal tree direction data index)))}
        :double-ema {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-double-ema-signal tree direction data index)))}
        :fisher {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-fisher-signal tree direction data index)))}
        :cci {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-cci-signal tree direction data index)))}
        :stoch {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-stoch-signal tree direction data index)))}
        :parabolic-sar {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-parabolic-sar-signal tree direction data index)))}
        :supertrend {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-supertrend-signal tree direction data index)))}
        :fibonacci {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-fibonacci-signal tree direction data index)))}
        :engulfing {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-engulfing-signal tree direction data index)))}
        :harami {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-harami-signal tree direction data index)))}
        :hammer {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-hammer-signal tree direction data index)))}
        :inverted-hammer {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-inverted-hammer-signal tree direction data index)))}
        :trend {index-keyword (strat/check-signal-with-window index (fn [index] (strat/check-trend-signal tree direction data index)))}
        :identity {index-keyword :identity}))))

(defn long?
  "Checks whether the generated individual signals result in a overall long signal for this strategy."
  [tree signals]
  (= :long (tree/signal-check tree signals :long)))

(defn short?
  "Checks whether the generated individual signals result in a overall short signal for this strategy."
  [tree signals]
  (= :short (tree/signal-check tree signals :short)))

;; Fitness calculation functions

(defn scale-profit-result
  "Scales the profit result to a positive value.
  Positive profits are given a multipler to give more incentive within the evolution.
  Profit value needs to be positive since it is used for weighted selection."
  [total-profit]
  (+ total-profit (:fitness-offset p/params)))

(defn calculate-total-profit
  "Calculates the total profit from given transactions."
  [transactions]
  (->> transactions
       (map :result)
       (reduce +)))

(defn calculate-accuracy-percentage
  "Calculates the accuracy percentage from given transactions."
  [transactions]
  (let [results (map :result transactions)
        profiting (count (filter (fn [res] (> res 0)) results))
        losing (count (filter (fn [res] (< res 0)) results))]
    (double (* 100 (/ profiting (+ profiting losing))))))

(defn normalize-profit
  "Normalizes total profit to be within 0-100 range for hybrid fitness calculation."
  [profit]
  (-> profit
      (* 100)
      (/ (:max-fitness-scale p/params))))

(defn calculate-profit-from-transactions
  "Calculates the total profit of given transactions."
  [transactions]
  (if-not (empty? transactions)
    (calculate-total-profit transactions)
    0))

(defn calculate-accuracy-profit-hybrid-from-transactions
  "Calculates a fitness based on profit and accuracy percentage, a maximum of 100 comes from the accuracy percentage, and a maximum of 100 comes from the total profit."
  [transactions]
  (if-not (empty? transactions)
    (let [total-profit (calculate-total-profit transactions)
          accuracy-percentage (calculate-accuracy-percentage transactions)
          accuracy-factor (:accuracy-factor p/params)
          profit-scale (* 2 (- 1 accuracy-factor))
          accuracy-scale (* 2 accuracy-factor)
          fitness (+ (* accuracy-scale accuracy-percentage)
                     (* profit-scale (normalize-profit total-profit)))]
      (if (> fitness 0)
        fitness
        1))
    1))

(defn calculate-accuracy-from-transactions
  "Calculates the total accurate count of given transactions."
  [transactions]
  (if-not (empty? transactions)
    (reduce + (->> transactions
                   (map :result)
                   (map (fn [res] (if (> res 0) 1 0)))))
    1))

(defn calculate-accuracy-percentage-from-transactions
  "Calculates the accuracy percentage of given transactions."
  [transactions]
  (if-not (empty? transactions)
    (calculate-accuracy-percentage transactions)
    1))

(defn select-fitness-fn-and-calculate
  "Calculates the total profit and scales it so that the result is positive."
  [transactions]
  (let [criterion (:fitness-criterion p/params)]
    (condp = criterion
      :profit (-> transactions
                  calculate-profit-from-transactions
                  scale-profit-result)
      :accuracy (calculate-accuracy-from-transactions transactions)
      :accuracy-percentage (calculate-accuracy-percentage-from-transactions transactions)
      :accuracy-profit-hybrid (calculate-accuracy-profit-hybrid-from-transactions transactions)
      (throw (IllegalArgumentException. "Unknown fitness function for fitness calculation")))))

;; Transaction generation functions

(defn create-transaction-map
  "Creates a transaction to be added to the transaction array
  Records the direction, time, and price"
  [data current-index direction]
  {:price (dg/get-bar-value-at-index data current-index)
   :position direction
   :bar-time (dg/get-bar-close-time-at-index data current-index)})

(defn merge-to-transaction
  "Merges the given entry and exit points into a transaction."
  [entry exit]
  {:post [(s/valid? :genetic/transaction %)]}
  (let [{:keys [capital leverage commission]} p/params
        position (:position entry)
        single-lot-result (- (:price exit) (:price entry))
        number-of-lots (/ capital (:price entry))
        result (- (* number-of-lots single-lot-result leverage) (* capital commission))]
    {:position position
     :result (if (= position :long) result (- result))
     :time-range (str (:bar-time entry) "-" (:bar-time exit))}))

(defn merge-entry-points
  "Merges transaction entry and exit points, then finds the profit of the transaction and records its time."
  [entry-points final-bar-value final-bar-end-time]
  (if-not (empty? entry-points)
    (loop [transactions []
           rem-entries entry-points]
      (if (> (count rem-entries) 1)
        (recur (conj transactions (merge-to-transaction (first rem-entries) (second rem-entries))) (rest rem-entries))
        (conj transactions (merge-to-transaction (first rem-entries) {:price final-bar-value :bar-time final-bar-end-time}))))
    []))

;; Strategy backtesting and real-time signal check functions

(defn backtest-strategy
  "Simulates the strategy given by the genetic-sequence on the data. Returns the final list of entry and exit points."
  [data genetic-sequence]
  (let [max-index (.getBarCount data)]
    (loop [current-position :none
           current-index 0
           entry-exit-points []]
      (if (< current-index max-index)
        (condp = current-position
          :long (let [short-signals (generate-signals (last genetic-sequence) :short current-index data)]
                  (if (short? (last genetic-sequence) short-signals)
                    (recur :short (inc current-index) (conj entry-exit-points (create-transaction-map data current-index :short)))
                    (recur current-position (inc current-index) entry-exit-points)))
          :short (let [long-signals (generate-signals (first genetic-sequence) :long current-index data)]
                   (if (long? (first genetic-sequence) long-signals)
                     (recur :long (inc current-index) (conj entry-exit-points (create-transaction-map data current-index :long)))
                     (recur current-position (inc current-index) entry-exit-points)))
          :none (let [long-signals (generate-signals (first genetic-sequence) :long current-index data)
                      short-signals (generate-signals (last genetic-sequence) :short current-index data)]
                  (cond
                    (short? (last genetic-sequence) short-signals) (recur :short (inc current-index) (conj entry-exit-points (create-transaction-map data current-index :short)))
                    (long? (first genetic-sequence) long-signals) (recur :long (inc current-index) (conj entry-exit-points (create-transaction-map data current-index :long)))
                    :else (recur current-position (inc current-index) entry-exit-points))))
        entry-exit-points))))

(defn check-signal-for-last-data
  "Generates the signals for the given strategy and returns the decision made."
  [data strategy current-position]
  (let [final-index (.getEndIndex data)]
    (condp = current-position
      :long (let [short-signals (generate-signals (last strategy) :short final-index data)]
              (if (short? (last strategy) short-signals)
                :short
                current-position))
      :short (let [long-signals (generate-signals (first strategy) :long final-index data)]
               (if (long? (first strategy) long-signals)
                 :long
                 current-position))
      :none (let [long-signals (generate-signals (first strategy) :long final-index data)
                  short-signals (generate-signals (last strategy) :short final-index data)]
              (cond
                (short? (last strategy) short-signals) :short
                (long? (first strategy) long-signals) :long
                :else current-position)))))

(defn calculate-fitness
  "Calculates the fitness of given genetic sequence."
  [data genetic-sequence]
  (let [max-index (-> data
                      .getBarCount
                      dec)
        entry-exit-points (backtest-strategy data genetic-sequence)
        transactions (merge-entry-points entry-exit-points
                                         (dg/get-bar-value-at-index data max-index)
                                         (dg/get-bar-close-time-at-index data max-index))]
    (select-fitness-fn-and-calculate transactions)))

(defn calculate-transactions-for-monitor
  "Calculates the transactions of given genetic sequence for bookkeeping."
  [data individual]
  (let [genetic-sequence (:genetic-sequence individual)
        max-index (-> data
                      .getBarCount
                      dec)]
    (merge-entry-points (backtest-strategy data genetic-sequence)
                        (dg/get-bar-value-at-index data max-index)
                        (dg/get-bar-close-time-at-index data max-index))))

(defn find-elitism-ind-count
  "Calculates the elite individual count from parameters for evolution."
  []
  (let [total (:population-size p/params)
        elite-ratio (:elitism-ratio p/params)]
    (math/ceil (* total elite-ratio))))

(defn start-evolution
  "Starts evolution, this method calls the nature library with the necessary params."
  [filenames & {:keys [population-size generation-count]
                :or {population-size (:population-size p/params)
                     generation-count (:generation-count p/params)}}]
  (let [evolution-id (str (uuid/v4))
        calculate-fitness-partial (partial calculate-fitness (dg/get-bars-for-genetic filenames :train))
        gen-count (atom 0)
        monitors [nmon/print-best-solution
                  mon/print-average-fitness-of-population
                  (fn [population current-generation] (mon/write-individuals-to-file-monitor evolution-id population current-generation))
                  (fn [population current-generation] (mon/write-transactions-to-file-monitor evolution-id (partial calculate-transactions-for-monitor (dg/get-bars-for-genetic filenames :test)) population current-generation))
                  (fn [population current-generation] (mon/save-fitnesses-to-file-for-current-generation evolution-id gen-count population current-generation))]]
    (tb/message-to-me (str "Starting evolution with id " evolution-id))
    (when-not (:in-container @env) (dyn/write-evolution-to-table evolution-id filenames))
    (n/evolve-with-sequence-generator generate-sequence
                                      population-size
                                      generation-count
                                      calculate-fitness-partial
                                      [(partial tree/crossover calculate-fitness-partial)]
                                      [(partial tree/mutation calculate-fitness-partial)]
                                      {:solutions 3
                                       :carry-over (find-elitism-ind-count)
                                       :insert-new (find-elitism-ind-count)
                                       :monitors monitors})))

