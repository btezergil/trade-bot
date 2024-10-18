(ns clojure-scraps.charts
  (:require [clojure-scraps.dynamo :as dyn]
            [oz.core :as oz]
            [clojure.tools.logging :as log]))

(oz/start-server!)

(defn get-evolution-stat-data
  [evolution-id]
  (flatten (map (fn [row] (list {:time (int (:generation-count row)) :item "average fitness" :quantity (double (:avg-fitness row))}
                                {:time (int (:generation-count row)) :item "best fitness" :quantity (double (:best-fitness row))}))
                (dyn/read-evolution-stats-from-table evolution-id))))

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
; TODO: yukaridaki statlari evolution-stats veya direk evolution tablosuna yazalim
(gather-statistics "b04084c0-9b9f-4ff8-bf96-cbc6e3d4d75a")

; TODO: bir strateji icin tum entry/exit point'leri al, sonrasinda bunlari grafik uzerinde isaretle, bununla birlikte de fiyatin grafigini ciz

;; Render the plot
(oz/view! (get-line-plot-map (get-evolution-stat-data "evolution id here")))
