(ns clojure-scraps.dynamo
  (:require [clj-uuid :as uuid]
            [clojure-scraps.aws :as aws-helper]
            [clojure-scraps.params :as p]
            [clojure-scraps.treenode :as node]
            [clojure.data.json :as json]
            [clojure.spec.alpha :as s]
            [clojure.walk :as walk]))

(def strategy-table-vars {:table-name "strategy-v1" :table-key "id"})
(def transaction-table-vars {:table-name "transaction-v1" :table-key "id"})
(def evolution-table-vars {:table-name "evolution-v1" :table-key "id"})

(defn read-individual-from-table
  "Queries the strategy-v1 table for the individual with the given id"
  [strategy-id]
  (let [read-from-table (aws-helper/read-from-table (:table-name strategy-table-vars) (:table-key strategy-table-vars) strategy-id)
        item (:Item read-from-table)
        {:keys [fitness genetic-sequence id age]} item]
    {:age (-> age
              :N
              Integer/parseInt)
     :fitness (-> fitness
                  :N
                  Double/parseDouble)
     :guid (:S id)
     :genetic-sequence (walk/postwalk node/keywordize-and-or
                                      (-> genetic-sequence
                                          :S
                                          (json/read-str :key-fn keyword :value-fn node/parse-json-values)))}))

(defn write-individual-to-table
  "Records the given genetic indivdual to database"
  [evolution-id individual]
  {:pre [s/valid? :genetic/individual individual]}
  (let [entry {"id" {:S (-> individual
                            :guid
                            str)}
               "evolution-id" {:S (str evolution-id)}
               "age" {:N (-> individual
                             :age
                             str)}
               "fitness" {:N (-> individual
                                 :fitness-score
                                 str)}
               "genetic-sequence" {:S (-> individual
                                          :genetic-sequence
                                          json/write-str)}}]
    (aws-helper/write-to-table (:table-name strategy-table-vars) entry)))

(defn write-transaction-to-table
  "Records the given transaction to database"
  [strategy-id transaction]
  (let [entry {"id" {:S (str (uuid/v4))}
               "strategy-id" {:S (str strategy-id)}
               "result" {:N (-> transaction
                                :result
                                str)}
               "position" {:S (-> transaction
                                  :position
                                  name)}
               "time-range" {:S (-> transaction
                                    :time-range
                                    str)}}]
    (aws-helper/write-to-table (:table-name transaction-table-vars) entry)))

(defn write-evolution-to-table
  "Records the evolution parameters to database with given id"
  [evolution-id]
  (let [entry {"id" {:S (str evolution-id)}
               "indicator-count" {:N (-> p/params
                                         :indicator-count
                                         str)}
               "window-size-min" {:N (-> p/params
                                         :window-size-min
                                         str)}
               "window-size-max" {:N (-> p/params
                                         :window-size-max
                                         str)}
               "crossover-probability" {:N (-> p/params
                                               :crossover-probability
                                               str)}
               "mutation-probability" {:N (-> p/params
                                              :mutation-probability
                                              str)}
               "flip-mutation-probability" {:N (-> p/params
                                                   :flip-mutation-probability
                                                   str)}
               "population-size" {:N (-> p/params
                                         :population-size
                                         str)}
               "rank-selection-offset" {:N (-> p/params
                                               :rank-selection-offset
                                               str)}
               "generation-count" {:N (-> p/params
                                          :generation-count
                                          str)}
               "elitism-ratio" {:N (-> p/params
                                       :elitism-ratio
                                       str)}
               "leverage" {:N (-> p/params
                                  :leverage
                                  str)}
               "capital" {:N (-> p/params
                                 :capital
                                 str)}
               "commission" {:N (-> p/params
                                    :commission
                                    str)}
               "stoploss-enabled" {:BOOL (-> p/params
                                             :stoploss-enabled
                                             str)}
               "takeprofit-enabled" {:BOOL (-> p/params
                                               :takeprofit-enabled
                                               str)}
               "stoploss-ratio" {:N (-> p/params
                                        :stoploss-ratio
                                        str)}
               "takeprofit-ratio" {:N (-> p/params
                                          :takeprofit-ratio
                                          str)}
               "data-window" {:N (-> p/params
                                     :data-window
                                     str)}
               "trading-window" {:N (-> p/params
                                        :trading-window
                                        str)}
               "trade-threshold" {:N (-> p/params
                                         :trade-threshold
                                         str)}
               "accuracy-profit-ratio" {:N (-> p/params
                                               :accuracy-profit-ratio
                                               str)}
               "prune-height" {:N (-> p/params
                                      :prune-height
                                      str)}
               "indicators" {:L (map (fn [x] {:S (name x)}) (:indicators p/params))}}]
    (aws-helper/write-to-table (:table-name evolution-table-vars) entry)))

