(ns clojure-scraps.params
  (:require [clojure-scraps.aws :as aws-helper]))

(def params
  {:indicator-count 5,
   :window-size-min 3,
   :window-size-max 15, ; TODO: window-size'i fix param olarak kullansak da training'e katmasak makul olur mu
   :crossover-probability 0.2,
   :mutation-probability 0.1,
   :flip-mutation-probability 0.25,
   :population-size 2,
   :rank-selection-offset 0.6,
   :generation-count 0,
   :elitism-percentage 0.05,
   :leverage 100,
   :capital 1000,
   :commission 0.005,
   :stoploss-enabled false,
   :takeprofit-enabled false,
   :stoploss-ratio 0.1,
   :takeprofit-ratio 0.2,
   :data-window 1000,
   :trading-window 100,
   :trade-threshold 0.05,
   :accuracy-profit-ratio 0.01,
   :indicators [:rsi :sma :ema :fisher :fibonacci :engulfing :pinbar],
   :prune-height 2,
   :default-age 0})

(def table-vars {:table-name "evolution-v1", :table-key "id"})

(defn write-evolution-to-table
  "Records the evolution parameters to database with given id"
  [evolution-id]
  (let [entry {"id" {:S (str evolution-id)},
               "indicator-count" {:N (-> params
                                         :indicator-count
                                         str)},
               "window-size-min" {:N (-> params
                                         :window-size-min
                                         str)},
               "window-size-max" {:N (-> params
                                         :window-size-max
                                         str)},
               "crossover-probability" {:N (-> params
                                               :crossover-probability
                                               str)},
               "mutation-probability" {:N (-> params
                                              :mutation-probability
                                              str)},
               "flip-mutation-probability" {:N (-> params
                                                   :flip-mutation-probability
                                                   str)},
               "population-size" {:N (-> params
                                         :population-size
                                         str)},
               "rank-selection-offset" {:N (-> params
                                               :rank-selection-offset
                                               str)},
               "generation-count" {:N (-> params
                                          :generation-count
                                          str)},
               "elitism-percentage" {:N (-> params
                                            :elitism-percentage
                                            str)},
               "leverage" {:N (-> params
                                  :leverage
                                  str)},
               "capital" {:N (-> params
                                 :capital
                                 str)},
               "commission" {:N (-> params
                                    :commission
                                    str)},
               "stoploss-enabled" {:BOOL (-> params
                                             :stoploss-enabled
                                             str)},
               "takeprofit-enabled" {:BOOL (-> params
                                               :takeprofit-enabled
                                               str)},
               "stoploss-ratio" {:N (-> params
                                        :stoploss-ratio
                                        str)},
               "takeprofit-ratio" {:N (-> params
                                          :takeprofit-ratio
                                          str)},
               "data-window" {:N (-> params
                                     :data-window
                                     str)},
               "trading-window" {:N (-> params
                                        :trading-window
                                        str)},
               "trade-threshold" {:N (-> params
                                         :trade-threshold
                                         str)},
               "accuracy-profit-ratio" {:N (-> params
                                               :accuracy-profit-ratio
                                               str)},
               "prune-height" {:N (-> params
                                      :prune-height
                                      str)},
               "indicators" {:L (map (fn [x] {:S (name x)}) (:indicators params))}}]
    (aws-helper/write-to-table (:table-name table-vars) entry)))
