(ns clojure-scraps.indicators.pinbar
  (:require [clojure.tools.logging :as log]
            [clojure-scraps.datagetter :as datagetter]
            [clojure.spec.alpha :as s])
  (:import (java.time ZoneId ZonedDateTime)
           (java.time.format DateTimeFormatter)
           [org.ta4j.core BarSeries Bar]
           [org.ta4j.core.num DecimalNum]
           [org.ta4j.core.indicators CachedIndicator]))


(defn create-hammer-indicator
  "Creates an instance of hammer indicator on the given set of bars"
  [bars]
  (proxy 
    [CachedIndicator]
    [bars]
    (calculate [index]
      (let [bar (.getBar (.getBarSeries this) index)
            open (.getOpenPrice bar)
            high (.getHighPrice bar)
            low (.getLowPrice bar)
            close (.getClosePrice bar)
            bar-percentage-change (.doubleValue (.abs (.minus (.dividedBy open close) (DecimalNum/valueOf 1))))
            bar-percentage-upside (.doubleValue (.abs (.minus (.dividedBy open high) (DecimalNum/valueOf 1)))) 
            bar-percentage-downside (.doubleValue (.abs (.minus (.dividedBy open low) (DecimalNum/valueOf 1))))]
        (and 
          (>= bar-percentage-change 0.005)
          (<= bar-percentage-upside 0.005) 
          (>= bar-percentage-downside 0.03))))))

