(ns clojure-scraps.stats
  (:require [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.params :as p]
            [clojure-scraps.datagetter :as dg]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]
            [clojure.pprint :as pp])
  (:import [java.time.format DateTimeFormatter]
           (java.time ZonedDateTime)))

(def operation-mode :file)
(def stat-file-folder "out-files/400gen/3height/hybrid/")
(def fitness-criterion :accuracy-percentage)

(defn read-individuals-from-file
  "Reads the individual file written by monitor function"
  [evolution-id]
  (with-open [rdr (io/reader (str stat-file-folder evolution-id "-individuals.txt"))]
    (let [lines (line-seq rdr)]
      (doall (map read-string lines)))))

(defn read-transactions-from-file
  "Reads the transactions file written by monitor function"
  [evolution-id]
  (with-open [rdr (io/reader (str stat-file-folder evolution-id "-transactions.txt"))]
    (let [lines (line-seq rdr)]
      (doall (map read-string lines)))))

(defn read-fitnesses-from-file
  "Reads the fitnesses file written by monitor function"
  [evolution-id]
  (with-open [rdr (io/reader (str stat-file-folder evolution-id "-fitnesses.txt"))]
    (let [lines (line-seq rdr)]
      (doall (map read-string lines)))))

(defn get-strategy-to-transaction-map
  [evolution-id]
  (reduce #(assoc %1 (:strategy-id %2) (:transactions %2)) {} (read-transactions-from-file evolution-id)))

(defn- get-evolution-stats
  [evolution-id]
  (condp = operation-mode
    :db (dyn/read-evolution-stats-from-table evolution-id)
    :file (read-fitnesses-from-file evolution-id)))

(defn- get-strategies-of-evolution
  [evolution-id]
  (condp = operation-mode
    :db (dyn/read-strategies-of-evolution evolution-id)
    :file (read-individuals-from-file evolution-id)))

(defn- get-transactions-of-strategy
  [evolution-id strategy-id]
  (condp = operation-mode
    :db (dyn/read-transactions-of-strategy strategy-id)
    :file (let [all-transactions (read-transactions-from-file evolution-id)]
            (filter #(= (:strategy-id %) strategy-id) all-transactions))))

(defn- get-transactions
  [evolution-id]
  (condp = operation-mode
    :db (dyn/read-transactions-from-table evolution-id)
    :file (read-transactions-from-file evolution-id)))

(defn extract-evolution-fitness-data
  "Given a list of evolution ids, gets the average and best fitness data and averages them."
  [evolution-ids]
  (let [evolution-stats (map get-evolution-stats evolution-ids)
        evolution-count (count evolution-ids)
        generation-count (count (first evolution-stats))]
    (loop [index 0
           data []]
      (if (>= index generation-count)
        data
        (recur (inc index) (conj data {:generation-count index
                                       :avg-fitness (/ (->> evolution-stats
                                                            (map #(nth % index))
                                                            (map :avg-fitness)
                                                            (reduce +)
                                                            double) evolution-count)
                                       :best-fitness (/ (->> evolution-stats
                                                             (map #(nth % index))
                                                             (map :best-fitness)
                                                             (reduce +)
                                                             double) evolution-count)}))))))

(defn- process-generation-data-for-violin-plot
  [data]
  (let [necessary-fields (dissoc data :best-fitness :avg-fitness)]
    (map (fn [dt] {:fitness dt :current-generation (:generation-count necessary-fields)}) (:fitness-list necessary-fields))))

(defn extract-evolution-all-fitness-data
  "Given a list of evolution ids, gets all fitness data associated with their current generation for violin plot."
  [evolution-ids]
  (->> evolution-ids
       (map get-evolution-stats)
       flatten
       (map process-generation-data-for-violin-plot)
       flatten))

(defn calculate-total-fitness-from-transactions
  "Calculates the total fitness of the strategy by adding all transaction results belonging to it."
  ([strategy evolution-id]
   (->> strategy
        :id
        (get-transactions-of-strategy evolution-id)
        (map :result)
        (reduce +)))

  ([transactions]
   (->> transactions
        (map :result)
        (reduce +))))

(defn generate-strategy-map-for-histogram-from-db
  "Given a strategy, generates the map for histogram/scatter plot. This method works for data from DB."
  ([evolution-id strategy]
   (list {:fitness (double (if (= :profit fitness-criterion)
                             (- (:fitness-score strategy) (:fitness-offset p/params))
                             (:fitness-score strategy)))
          :profit (double (calculate-total-fitness-from-transactions strategy evolution-id))})))

(defn generate-strategy-map-for-histogram-from-file
  "Given a strategy, generates the map for histogram/scatter plot. This method works for data from files."
  [transactions strategy]
  (list {:fitness (double (if (= :profit fitness-criterion)
                            (- (:fitness-score strategy) (:fitness-offset p/params))
                            (:fitness-score strategy)))
         :profit (double (calculate-total-fitness-from-transactions transactions))}))

(defn extract-histogram-data
  "Given an evolution id, gets the fitness and profit data for its strategies."
  [evolution-id]
  (let [strategies (get-strategies-of-evolution evolution-id)]
    (condp = operation-mode
      :db (flatten (map #(generate-strategy-map-for-histogram-from-db evolution-id %) strategies))
      :file (let [strategy-to-transaction-map (get-strategy-to-transaction-map evolution-id)]
              (flatten (map #(generate-strategy-map-for-histogram-from-file (get strategy-to-transaction-map (str (:guid %))) %) strategies))))))

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
                (extract-evolution-fitness-data evolution-ids))))

(defn get-evolution-stat-data-accuracy
  "Given a list of evolution ids, gets the average and best fitness data."
  [evolution-ids]
  (flatten (map (fn [row] (list {:generation (int (:generation-count row)) :item "average fitness" :fitness (-> row
                                                                                                                :avg-fitness
                                                                                                                double)}
                                {:generation (int (:generation-count row)) :item "best fitness" :fitness (-> row
                                                                                                             :best-fitness
                                                                                                             double)}))
                (extract-evolution-fitness-data evolution-ids))))

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
  (let [transactions (get-transactions evolution-id)
        strategy-count (count (get-strategies-of-evolution evolution-id))]
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
  (let [strategies (get-strategies-of-evolution evolution-id)
        best-strat (reduce (fn [str1 str2] (if (> (:fitness-score str1) (:fitness-score str2)) str1 str2)) strategies)]
    (when all (log/info (pp/pprint (map (fn [strategy] (str "strategy-id: " (:id strategy)
                                                            ", fitness: " (if (= :profit (:fitness-criterion p/params))
                                                                            (- (:fitness-score strategy) (:fitness-offset p/params))
                                                                            (:fitness-score strategy))
                                                            ", test fitness: " (calculate-total-fitness-from-transactions strategy evolution-id)))
                                        strategies))))
    (log/info (pp/pprint  (str "Best strategy: " best-strat
                               ", test fitness: " (calculate-total-fitness-from-transactions  best-strat evolution-id))))))

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
  [evolution-id strategy-id]
  (merge-bars-with-transactions (convert-bars-to-ohlc (dg/get-bars-for-genetic dg/evolution-filenames-map :test))
                                (-> strategy-id
                                    (get-transactions-of-strategy evolution-id)
                                    get-entry-data-from-transactions)))

;(compare-training-and-test-performance "f0b63ccb-d808-4b8e-9253-eba6a6c1c11b")
;(extract-histogram-data "ef89578c-ae37-43a1-a0f5-7c805d2c5e8a")

;(:strategy-id (first (read-transactions-from-file "643c4d0f-7be8-463f-a7e2-80becfcb1703")))


