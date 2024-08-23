(ns clojure-scraps.indicators.pinbar
  (:require
   [clojure.tools.logging :as log]
   [clojure-scraps.datagetter :as datagetter]
   [clojure.spec.alpha :as s])
  (:import
   [org.ta4j.core.num DecimalNum]
   [org.ta4j.core.indicators CachedIndicator]))

(defn bar-close-ratio
  "Calculates the ratio between open and close prices of a bar"
  [bar]
  (let [open (.getOpenPrice bar)
        close (.getClosePrice bar)]
    (-> (.dividedBy open close)
        (.minus (DecimalNum/valueOf 1))
        .abs
        .doubleValue)))

(defn bar-upside-ratio
  "Calculates the ratio between open and high prices of a bar"
  [bar]
  (let [open (.getOpenPrice bar)
        high (.getClosePrice bar)]
    (-> (.dividedBy open high)
        (.minus (DecimalNum/valueOf 1))
        .abs
        .doubleValue)))

(defn bar-downside-ratio
  "Calculates the ratio between open and low prices of a bar"
  [bar]
  (let [open (.getOpenPrice bar)
        low (.getClosePrice bar)]
    (-> (.dividedBy open low)
        (.minus (DecimalNum/valueOf 1))
        .abs
        .doubleValue)))

(s/def :pinbar/is-hammer
  (s/and
   #(= (type %) org.ta4j.core.BaseBar)
   #(>= (bar-close-ratio %) 0.005)
   #(<= (bar-close-ratio %) 0.01)
   #(<= (bar-upside-ratio %) 0.005)
   #(>= (bar-downside-ratio %) 0.025)))

(s/def :pinbar/is-shooting-star
  (s/and
   #(= (type %) org.ta4j.core.BaseBar)
   #(>= (bar-close-ratio %) 0.005)
   #(<= (bar-close-ratio %) 0.01)
   #(>= (bar-upside-ratio %) 0.025)
   #(<= (bar-downside-ratio %) 0.005)))

(defn create-hammer-indicator
  "Creates an instance of hammer indicator on the given set of bars"
  [bars]
  (proxy
   [CachedIndicator]
   [bars]
    (calculate [index]
      (let [bar (.getBar (.getBarSeries this) index)
            bar-percentage-change (bar-close-ratio bar)
            bar-percentage-upside (bar-upside-ratio bar)
            bar-percentage-downside (bar-downside-ratio bar)]
        (and
         (>= bar-percentage-change 0.005)
         (<= bar-percentage-change 0.01)
         (<= bar-percentage-upside 0.005)
         (>= bar-percentage-downside 0.025))))))

(defn create-shooting-star-indicator
  "Creates an instance of shooting star indicator on the given set of bars"
  [bars]
  (proxy
   [CachedIndicator]
   [bars]
    (calculate [index]
      (let [bar (.getBar (.getBarSeries this) index)
            bar-percentage-change (bar-close-ratio bar)
            bar-percentage-upside (bar-upside-ratio bar)
            bar-percentage-downside (bar-downside-ratio bar)]
        (and
         (>= bar-percentage-change 0.005)
         (<= bar-percentage-change 0.01)
         (<= bar-percentage-downside 0.005)
         (>= bar-percentage-upside 0.025))))))

(defn is-hammer?
  [open high low close]
  (let [bar-percentage-change (abs (- 1 (/ close open)))
        bar-percentage-upside  (abs (- 1 (/ high open)))
        bar-percentage-downside (abs (- 1 (/ low open)))]
    (and
     (>= bar-percentage-change 0.005)
     (<= bar-percentage-change 0.01)
     (<= bar-percentage-upside 0.005)
     (>= bar-percentage-downside 0.025))))

;(is-hammer? 380.43 380.63 349.56 350.42)
;(filter #(true? (second %)) (map #(list % (s/valid? :pinbar/is-hammer %)) (.getBarData (datagetter/get-bars))))
; TODO: false olanlari filter-out'la, gercekten hammer bulabiliyor mu bi gorelim

