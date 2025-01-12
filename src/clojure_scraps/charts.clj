(ns clojure-scraps.charts
  (:require [clojure-scraps.stats :as st]
            [oz.core :as oz]))

(defn get-line-plot-map-profit
  "Data should be in a map with :time, :item, and :quantity fields."
  [data]
  {:width 1400
   :height 800
   :data {:values data}
   :encoding {:x {:field "generation" :type "quantitative"}
              :y {:field "fitness" :type "quantitative"}
              :color {:field "item" :type "nominal"}}
   :mark "line"})

(defn get-line-plot-map-accuracy
  "Data should be in a map with :time, :item, and :quantity fields."
  [data]
  {:width 1400
   :height 800
   :data {:values data}
   :encoding {:x {:field "time" :type "quantitative"}
              :y {:field "quantity" :type "quantitative" :scale {:domain [0 850]}}
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

; TODO: bir strateji icin tum entry/exit point'leri al, sonrasinda bunlari grafik uzerinde isaretle, bununla birlikte de fiyatin grafigini ciz

(oz/start-server!)

(defn profit-fitness-plot
  [evolution-ids]
  (-> evolution-ids
      st/get-evolution-stat-data
      get-line-plot-map-profit))

(defn accuracy-fitness-plot
  [evolution-ids]
  (-> evolution-ids
      st/get-evolution-stat-data
      get-line-plot-map-accuracy))

(defn histogram-plot
  [evolution-id]
  (-> evolution-id
      st/extract-histogram-data
      get-histogram-map))

(defn scatter-plot
  [evolution-id]
  (-> evolution-id
      st/extract-histogram-data
      get-scatterplot-map))

;; Render the plot
(oz/view! (scatter-plot "67bdcb0f-0d4f-4ad7-b5ac-d47a7324de3b"))
(oz/view! (histogram-plot "67bdcb0f-0d4f-4ad7-b5ac-d47a7324de3b"))
(oz/view! (profit-fitness-plot ["ef89578c-ae37-43a1-a0f5-7c805d2c5e8a"]))
(oz/view! (accuracy-fitness-plot ["67bdcb0f-0d4f-4ad7-b5ac-d47a7324de3b"]))
