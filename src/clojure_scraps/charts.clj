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

(defn get-violinplot-map
  "Data should be in a map with :fitness and :profit fields."
  [data]
  {:data [{:name "fitnesses"
           :values data}
          {:name "density"
           :source "fitnesses"
           :transform [{:type "kde"
                        :field "fitness"
                        :groupby ["current-generation"]
                        :bandwidth 0}]}
          {:name "stats"
           :source "fitnesses"
           :transform [{:type "aggregate"
                        :fields ["fitness" "fitness" "fitness"]
                        :groupby ["current-generation"]
                        :ops ["q1" "median" "q3"]
                        :as ["q1" "median" "q3"]}]}]

   :scales [{:name "layout"
             :type "band"
             :range "height"
             :domain {:data "fitnesses" :field "current-generation"}}
            {:name "xscale"
             :type "linear"
             :range "width" :round true
             :domain {:data "fitnesses" :field "fitness"}
             :domainMin 1
             :zero false :nice true}
            {:name "hscale"
             :type "linear"
             :range [0 {:signal "plotWidth"}]
             :domain {:data "density" :field "density"}}
            {:name "color"
             :type "ordinal"
             :domain {:data "fitnesses" :field "current-generation"}
             :range "category"}]

   :axes [{:orient "bottom" :scale "xscale" :zindex 1}
          {:orient "left" :scale "layout" :tickCount 5 :zindex 1}]

   :marks [{:type "group"
            :from {:facet {:data "density"
                           :name "violin"
                           :groupby "current-generation"}}
            :encode {:enter {:yc {:scale "layout"
                                  :field "current-generation"
                                  :band 0.5}
                             :height {:signal "plotWidth"}
                             :width {:signal "width"}}}
            :data [{:name "summary"
                    :source "stats"
                    :transform [{:expr "datum.current-generation == parent.current-generation"}]}]

            :marks [{:type "area"
                     :from {:data "violin"}
                     :encode {:enter {:fill {:scale "color"
                                             :field {:parent "current-generation"}}}
                              :update {:x {:scale "xscale" :field "value"}
                                       :yc {:signal "plotWidth/2"}
                                       :height {:scale "hscale" :field "density"}}}}
                    {:type "rect"
                     :from {:data "summary"}
                     :encode {:enter {:fill {:value "black"}
                                      :height {:value 2}}
                              :update {:x {:scale "xscale" :field "q1"}
                                       :x2 {:scale "xscale" :field "q3"}
                                       :yc {:signal "plotWidth/2"}}}}
                    {:type "rect"
                     :from {:data "summary"}
                     :encode {:enter {:fill {:value "black"}
                                      :width {:value 2}
                                      :height {:value 8}}
                              :update {:x {:scale "xscale" :field "median"}
                                       :yc {:signal "plotWidth/2"}}}}]}]})

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
  [evolution-ids]
  (->> evolution-ids
       (map st/extract-histogram-data)
       flatten
       get-histogram-map))

(defn scatter-plot
  [evolution-ids]
  (->> evolution-ids
       (map st/extract-histogram-data)
       flatten
       get-scatterplot-map))

(defn violin-plot
  [evolution-ids]
  (-> evolution-ids
      st/extract-evolution-all-fitness-data
      get-violinplot-map))

(defn histogram-plot-single
  [evolution-id]
  (-> evolution-id
      st/extract-histogram-data
      get-histogram-map))

(defn scatter-plot-single
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
;(oz/view! (scatter-plot res/accuracy-100pop-400gen-3height-ids))
;(oz/view! (histogram-plot res/accuracy-100pop-400gen-4height-ids))
;(oz/view! (profit-fitness-plot res/accuracy-perc-100pop-200gen-3height-ids))
(oz/view! (violin-plot res/hybrid-new-ids) :mode :vega)
