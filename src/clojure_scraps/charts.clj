(ns clojure-scraps.charts
  (:require [clojure-scraps.dynamo :as dyn]
            [oz.core :as oz]))

(oz/start-server!)

(def data (flatten (map (fn [row] (list {:time (int (:generation-count row)) :item "average fitness" :quantity (double (:avg-fitness row))}
                                        {:time (int (:generation-count row)) :item "best fitness" :quantity (double (:best-fitness row))}))
                        (dyn/read-evolution-stats-from-table "4b8bda8a-9f08-495c-ac85-97bde86ab9ee"))))

(def line-plot
  {:width 1400
   :height 800
   :data {:values data}
   :encoding {:x {:field "time" :type "quantitative"}
              :y {:field "quantity" :type "quantitative" :scale {:domain [18000 40000]}}
              :color {:field "item" :type "nominal"}}
   :mark "line"})

;; Render the plot
(oz/view! line-plot)
