(ns clojure-scraps.charts
  (:require [clojure-scraps.dynamo :as dyn]
            [oz.core :as oz]
            [clojure.tools.logging :as log]))

(oz/start-server!)

(defn extract-evolution-data
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

(defn get-evolution-stat-data
  "Given a list of evolution ids, gets the average and best fitness data, and draws a line graph for these two fields."
  [evolution-ids]
  (flatten (map (fn [row] (list {:time (int (:generation-count row)) :item "average fitness" :quantity (double (:avg-fitness row))}
                                {:time (int (:generation-count row)) :item "best fitness" :quantity (double (:best-fitness row))}))
                (extract-evolution-data evolution-ids))))

(defn get-line-plot-map
  "Data should be in a map with :time, :item, and :quantity fields"
  [data]
  {:width 1400
   :height 800
   :data {:values data}
   :encoding {:x {:field "time" :type "quantitative"}
              :y {:field "quantity" :type "quantitative" :scale {:domain [18000 42000]}}
              :color {:field "item" :type "nominal"}}
   :mark "line"})

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
;(gather-statistics "95fb503c-8294-4e55-85eb-30e56131ad96")

; TODO: bir strateji icin tum entry/exit point'leri al, sonrasinda bunlari grafik uzerinde isaretle, bununla birlikte de fiyatin grafigini ciz
(def evolution-ids ["ee65b514-a389-42f8-a5d4-28452aec29e0"])
;; Render the plot
(oz/view! (-> evolution-ids
              get-evolution-stat-data
              get-line-plot-map))
