(ns clojure-scraps.stats
  (:require [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.params :as p]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp]))

(defn extract-evolution-stat-data
  "Given a list of evolution ids, gets the average and best fitness data and averages them."
  [evolution-ids]
  (let [evolution-stats (map dyn/read-evolution-stats-from-table evolution-ids)
        evolution-count (count evolution-ids)
        generation-count (count (first evolution-stats))]
    (loop [index 0
           data []]
      (if (>= index generation-count)
        data
        (recur (inc index) (conj data {:generation-count index
                                       :avg-fitness (/ (->> evolution-stats
                                                            (map #(get % index))
                                                            (map :avg-fitness)
                                                            (reduce +)
                                                            double) evolution-count)
                                       :best-fitness (/ (->> evolution-stats
                                                             (map #(get % index))
                                                             (map :best-fitness)
                                                             (reduce +)
                                                             double) evolution-count)}))))))

(defn calculate-total-fitness-from-transactions
  "Calculates the total fitness of the strategy by adding all transaction results belonging to it."
  [strategy]
  (->> strategy
       :id
       (dyn/read-transactions-of-strategy)
       (map :result)
       (reduce +)))

(defn extract-histogram-data
  "Given an evolution id, gets the fitness and profit data for its strategies."
  [evolution-id]
  (let [strategies (dyn/read-strategies-of-evolution evolution-id)
        fitness-criterion (:fitness-criterion (dyn/read-evolution-from-table evolution-id))]
    (flatten (map (fn [strategy] (list {:fitness (double (if (= :profit fitness-criterion)
                                                           (- (:fitness strategy) (:fitness-offset p/params))
                                                           (:fitness strategy)))
                                        :profit (double (calculate-total-fitness-from-transactions strategy))}))
                  strategies))))

(defn get-evolution-stat-data
  "Given a list of evolution ids, gets the average and best fitness data."
  [evolution-ids]
  (flatten (map (fn [row] (list {:generation (int (:generation-count row)) :item "average fitness" :fitness (-> row
                                                                                                                :avg-fitness
                                                                                                                double
                                                                                                                (- (:fitness-offset p/params)))}
                                {:generation (int (:generation-count row)) :item "best fitness" :fitness (-> row
                                                                                                             :best-fitness
                                                                                                             double
                                                                                                             (- (:fitness-offset p/params)))}))
                (extract-evolution-stat-data evolution-ids))))

(defn report-statistics-and-save-to-db
  "Saves the gathered statistics to database and prints them to logs."
  [evolution-id transaction-count long-transaction-count short-transaction-count profiting-long profiting-short total-profit strategy-count]
  (dyn/write-evolution-final-stats-to-table evolution-id {:total-transaction-count transaction-count
                                                          :long-transaction-count long-transaction-count
                                                          :short-transaction-count short-transaction-count
                                                          :profiting-long profiting-long
                                                          :profiting-short profiting-short
                                                          :total-profit total-profit
                                                          :strategy-count strategy-count})
  (log/info "Total transactions:" (double (/ transaction-count strategy-count)))
  (log/info "Long transactions:" (double (/ long-transaction-count strategy-count)))
  (log/info "Short transactions:" (double (/ short-transaction-count strategy-count)))
  (log/info "Profiting long transactions:" (double (/ profiting-long strategy-count)))
  (log/info "Profiting short transactions:" (double (/ profiting-short strategy-count)))
  (log/info "Total profit:" (/ (double total-profit) strategy-count)))

(defn gather-statistics
  "Gathers transaction-related statistics from the database."
  [evolution-id]
  (let [transactions (dyn/read-transactions-from-table evolution-id)
        strategy-count (count (dyn/read-strategies-of-evolution evolution-id))]
    (loop [transaction-count 0
           long-transaction-count 0
           short-transaction-count 0
           profiting-long 0
           profiting-short 0
           total-profit 0
           remaining-transactions transactions]
      (if (not-empty remaining-transactions)
        (let [transaction (first remaining-transactions)
              position (:position transaction)
              result (:result transaction)
              time-range (:time-range transaction)]
          (if (= position "long")
            (if (> result 0)
              (recur (inc transaction-count)
                     (inc long-transaction-count)
                     short-transaction-count
                     (inc profiting-long)
                     profiting-short
                     (+ total-profit result)
                     (rest remaining-transactions))
              (recur (inc transaction-count)
                     (inc long-transaction-count)
                     short-transaction-count
                     profiting-long
                     profiting-short
                     (+ total-profit result)
                     (rest remaining-transactions)))
            (if (> result 0)
              (recur (inc transaction-count)
                     long-transaction-count
                     (inc short-transaction-count)
                     profiting-long
                     (inc profiting-short)
                     (+ total-profit result)
                     (rest remaining-transactions))
              (recur (inc transaction-count)
                     long-transaction-count
                     (inc short-transaction-count)
                     profiting-long
                     profiting-short
                     (+ total-profit result)
                     (rest remaining-transactions)))))
        (report-statistics-and-save-to-db evolution-id transaction-count long-transaction-count short-transaction-count profiting-long profiting-short total-profit strategy-count)))))

(defn compare-training-and-test-performance
  "Reports the training and test fitnesses of every strategy. 
  Training fitness is on the strategy itself, while the test fitness is calculated from the transactions."
  [evolution-id & {:keys [all]
                   :or {all false}}]
  (let [strategies (dyn/read-strategies-of-evolution evolution-id)
        best-strat (reduce (fn [str1 str2] (if (> (:fitness str1) (:fitness str2)) str1 str2)) strategies)]
    (when all (log/info (pp/pprint (map (fn [strategy] (str "strategy-id: " (:id strategy)
                                                            ", fitness: " (if (= :profit (:fitness-criterion p/params))
                                                                            (- (:fitness strategy) (:fitness-offset p/params))
                                                                            (:fitness strategy))
                                                            ", test fitness: " (calculate-total-fitness-from-transactions strategy)))
                                        strategies))))
    (log/info (pp/pprint  (str "Best strategy: " best-strat
                               ", test fitness: " (calculate-total-fitness-from-transactions  best-strat))))))

(compare-training-and-test-performance "67bdcb0f-0d4f-4ad7-b5ac-d47a7324de3b")
(extract-histogram-data "ef89578c-ae37-43a1-a0f5-7c805d2c5e8a")
(gather-statistics "67bdcb0f-0d4f-4ad7-b5ac-d47a7324de3b")
