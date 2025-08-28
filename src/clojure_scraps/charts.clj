(ns clojure-scraps.charts
  (:require [clojure-scraps.stats :as st]
            [clojure-scraps.results :as res]
            [oz.core :as oz]
            [clojure-scraps.dynamo :as dyn]))

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
   :encoding {:x {:field "generation" :type "quantitative"}
              :y {:field "fitness" :type "quantitative"}
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
  (let [best-val (reduce (fn [d1 d2] (if (> (:fitness d1) (:fitness d2))
                                       d1 d2)) data)]
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
                         :color {:value "red"}}}
             {:mark "point"
              :encoding {:x {:datum (:fitness best-val)}
                         :y {:datum (:profit best-val)}
                         :color {:value "red"}}}]}))

(defn get-candlestick-map
  "Candlestick map to be drawn by Oz."
  [data]
  {:data {:values data}
   :width 2000
   :height 800
   :encoding {:x {:field "date" :type "ordinal" :timeUnit "utcyearmonthdatehoursminutes" :axis {:format "%Y-%m-%dT%H:%M:%SZ" :labelAngle -45}}
              :y {:type "quantitative" :scale {:zero false} :axis {:title "Price"}}
              :color {:condition {:test "datum.open < datum.close"
                                  :value "#06982d"}
                      :value "#ae1325"}}
   :layer [{:mark "rule"
            :encoding {:y {:field "low"}
                       :y2 {:field "high"}}}
           {:mark "bar"
            :encoding {:y {:field "open"}
                       :y2 {:field "close"}}}
           {:transform [{:filter "datum.long == true"}]
            :mark "rule"
            :encoding {:x {:field "date"}
                       :color {:value "#0d16bd"}}}
           {:transform [{:filter "datum.long == false"}]
            :mark "rule"
            :encoding {:x {:field "date"}
                       :color {:value "#f5b342"}}}
           {:params [{:name "hover"
                      :select {:type "point"
                               :fields ["date"]
                               :nearest true
                               :on "mouseover"}}]
            :mark "rule"
            :encoding {:tooltip [{:field "open" :type "quantitative"}
                                 {:field "high" :type "quantitative"}
                                 {:field "low" :type "quantitative"}
                                 {:field "close" :type "quantitative"}
                                 {:field "date" :type "ordinal" :timeUnit "utcyearmonthdatehoursminutes"}]
                       :color {:value  "black"}
                       :opacity {:condition {:value 0.6
                                             :param "hover"
                                             :empty false}
                                 :value 0}}}]})

(oz/start-server!)

(defn profit-fitness-plot
  [evolution-ids]
  (-> evolution-ids
      st/get-evolution-stat-data-profit
      get-line-plot-map-profit))

(defn accuracy-fitness-plot
  [evolution-ids]
  (-> evolution-ids
      st/get-evolution-stat-data-accuracy
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

(defn candlestick-data-from-evolution-id
  [evolution-id]
  (->> evolution-id
       dyn/read-strategies-of-evolution
       (reduce (fn [s1 s2] (if (> (:fitness s1) (:fitness s2)) s1 s2)))
       :id
       (st/get-candlestick-data evolution-id)
       get-candlestick-map))

;; Render the plot
;(oz/view! (candlestick-data-from-evolution-id  "4fd03e75-e552-4c65-a377-02cd4f9ccc91"))
(oz/view! (scatter-plot (nth res/profit-100pop-300gen-3height-ids 0)))
(oz/view! (histogram-plot (nth res/profit-100pop-300gen-3height-ids 0)))
(oz/view! (profit-fitness-plot res/profit-100pop-300gen-3height-ids))
;(oz/view! (accuracy-fitness-plot res/accuracy-perc-100pop-100gen-3height-ids))
