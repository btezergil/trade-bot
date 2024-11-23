(ns clojure-scraps.charts
  (:require [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.params :as p]
            [oz.core :as oz]
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
  (let [strategies (dyn/read-strategies-of-evolution evolution-id)]
    (flatten (map (fn [strategy] (list {:fitness (double (if (= :profit (:fitness-criterion p/params))
                                                           (- (:fitness strategy) (:fitness-offset p/params))
                                                           (:fitness strategy)))
                                        :profit (double (calculate-total-fitness-from-transactions strategy))}))
                  strategies))))

(defn get-evolution-stat-data
  "Given a list of evolution ids, gets the average and best fitness data, and draws a line graph for these two fields."
  [evolution-ids]
  (flatten (map (fn [row] (list {:time (int (:generation-count row)) :item "average fitness" :quantity (double (:avg-fitness row))}
                                {:time (int (:generation-count row)) :item "best fitness" :quantity (double (:best-fitness row))}))
                (extract-evolution-stat-data evolution-ids))))

(defn get-line-plot-map
  "Data should be in a map with :time, :item, and :quantity fields."
  [data]
  {:width 1400
   :height 800
   :data {:values data}
   :encoding {:x {:field "time" :type "quantitative"}
              :y {:field "quantity" :type "quantitative" :scale {:domain [18000 42000]}}
              :color {:field "item" :type "nominal"}}
   :mark "line"})

(defn get-histogram-map
  "Data should be in a map with :fitness and :profit fields."
  [data]
  {:data {:values data}
   :transform [{:filter {:and [{:field "fitness" :valid true}
                               {:field "profit" :valid true}]}}]
   :width 1400
   :height 800
   :encoding {:x {:bin {:maxbins 14} :field "fitness" :type "quantitative"}
              :y {:bin {:maxbins 20} :field "profit" :type "quantitative"}}
   :layer [{:mark "rect"
            :encoding {:color {:aggregate "count" :type "quantitative"}}}
           {:mark {:type "text"
                   :fontSize 14}
            :encoding {:text {:aggregate "count" :type "quantitative"}
                       :color {:value "black"}}}]
   :config {:view {:stroke "transparent"}}})

(defn get-scatterplot-map
  "Data should be in a map with :fitness and :profit fields."
  [data]
  {:data {:values data}
   :transform [{:filter {:and [{:field "fitness" :valid true}
                               {:field "profit" :valid true}]}}]
   :width 1400
   :height 800
   :encoding {:x {:field "fitness" :type "quantitative"}
              :y {:field "profit" :type "quantitative"}}
   :layer [{:mark "point"}
           {:mark "line"
            :encoding {:y {:datum 0}
                       :color {:value "red"}}}]})

(defn gather-statistics
  "Gathers transaction-related statistics from the database."
  [evolution-id]
  (let [transactions (dyn/read-transactions-from-table evolution-id)]
    (loop [transaction-count 0
           long-transaction-count 0
           short-transaction-count 0
           profiting-long 0
           profiting-short 0
           total-profit 0
           remaining-transactions transactions]
      (log/info "transaction count:" transaction-count)
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
        (do (log/info "Total transactions:" transaction-count)
            (log/info "Long transactions:" long-transaction-count)
            (log/info "Short transactions:" short-transaction-count)
            (log/info "Profiting long transactions:" profiting-long)
            (log/info "Profiting short transactions:" profiting-short)
            (log/info "Total profit:" total-profit)
            (dyn/write-evolution-final-stats-to-table evolution-id {:total-transaction-count transaction-count
                                                                    :long-transaction-count long-transaction-count
                                                                    :short-transaction-count short-transaction-count
                                                                    :profiting-long profiting-long
                                                                    :profiting-short profiting-short
                                                                    :total-profit total-profit}))))))

(defn compare-training-and-test-performance
  "Reports the training and test fitnesses of every strategy. 
  Training fitness is on the strategy itself, while the test fitness is calculated from the transactions."
  [evolution-id]
  (let [strategies (dyn/read-strategies-of-evolution evolution-id)]
    (log/info (pp/pprint (map (fn [strategy] (str "strategy-id: " (:id strategy)
                                                  ", fitness: " (if (= :profit (:fitness-criterion p/params))
                                                                  (- (:fitness strategy) (:fitness-offset p/params))
                                                                  (:fitness strategy))
                                                  ", test fitness: " (calculate-total-fitness-from-transactions strategy)))
                              strategies)))))

;(compare-training-and-test-performance "944776c3-69f8-4fa8-b0ba-ea14e83f6228")
;(extract-histogram-data "944776c3-69f8-4fa8-b0ba-ea14e83f6228")
;(gather-statistics "95fb503c-8294-4e55-85eb-30e56131ad96")

; TODO: bir strateji icin tum entry/exit point'leri al, sonrasinda bunlari grafik uzerinde isaretle, bununla birlikte de fiyatin grafigini ciz

(oz/start-server!)

(defn fitness-plot
  [evolution-ids]
  (-> evolution-ids
      get-evolution-stat-data
      get-line-plot-map))

(defn histogram-plot
  [evolution-id]
  (-> evolution-id
      extract-histogram-data
      get-histogram-map))

(defn scatter-plot
  [evolution-id]
  (-> evolution-id
      extract-histogram-data
      get-scatterplot-map))

;(fitness-plot ["ee65b514-a389-42f8-a5d4-28452aec29e0"])

;; Render the plot
(oz/view! (scatter-plot "944776c3-69f8-4fa8-b0ba-ea14e83f6228"))
