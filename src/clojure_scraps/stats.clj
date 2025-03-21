(ns clojure-scraps.stats
  (:require [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.params :as p]
            [clojure-scraps.datagetter :as dg]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pp])
  (:import [java.time.format DateTimeFormatter]
           (java.time ZonedDateTime)))

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

(defn get-evolution-stat-data-profit
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

(defn get-evolution-stat-data-accuracy
  "Given a list of evolution ids, gets the average and best fitness data."
  [evolution-ids]
  (flatten (map (fn [row] (list {:generation (int (:generation-count row)) :item "average fitness" :fitness (-> row
                                                                                                                :avg-fitness
                                                                                                                double)}
                                {:generation (int (:generation-count row)) :item "best fitness" :fitness (-> row
                                                                                                             :best-fitness
                                                                                                             double)}))
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

(defn convert-bars-to-ohlc
  "Gets the bars and converts them to a map that can be used to draw graph in Oz."
  [bars]
  (map (fn [bar] {:open (-> bar
                            .getOpenPrice
                            .doubleValue)
                  :high (-> bar
                            .getHighPrice
                            .doubleValue)
                  :low (-> bar
                           .getLowPrice
                           .doubleValue)
                  :close (-> bar
                             .getClosePrice
                             .doubleValue)
                  :date (-> bar
                            .getBeginTime
                            (.format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME))})
       (.getBarData bars)))

(defn get-entry-data-from-transactions
  "Gets the begin time and position direction from given transactions."
  [transactions]
  (map (fn [transaction] {:long (if  (= (:position transaction) "long") true false)
                          :begin-time (-> transaction
                                          :time-range
                                          (subs 0 22) ; 0-22 for beginTime, 23-45 for endTime
                                          (ZonedDateTime/parse DateTimeFormatter/ISO_ZONED_DATE_TIME)
                                          (.format java.time.format.DateTimeFormatter/RFC_1123_DATE_TIME))}) transactions))

(defn merge-bars-with-transactions
  "Merges the OHLC bar data with transaction data to be shown on the graph."
  [ohlc transactions]
  (loop [bars ohlc
         entry-points transactions
         result []]
    (if (not-empty bars)
      (let [bar (first bars)
            entry-point (first entry-points)]
        (if (= (:date bar) (:begin-time entry-point))
          (recur (rest bars)
                 (rest entry-points)
                 (conj result (merge bar entry-point)))
          (recur (rest bars)
                 entry-points
                 (conj result bar))))
      result)))

(defn get-candlestick-data
  "Generates the OHLC data with entry points."
  [strategy-id]
  (merge-bars-with-transactions (convert-bars-to-ohlc (dg/get-bars-for-genetic dg/evolution-filenames-map :test))
                                (-> strategy-id
                                    dyn/read-transactions-of-strategy
                                    get-entry-data-from-transactions)))

;(compare-training-and-test-performance "f0b63ccb-d808-4b8e-9253-eba6a6c1c11b")
;(extract-histogram-data "ef89578c-ae37-43a1-a0f5-7c805d2c5e8a")
;(gather-statistics "f0b63ccb-d808-4b8e-9253-eba6a6c1c11b")

