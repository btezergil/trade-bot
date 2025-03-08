(ns clojure-scraps.datagetter
  (:require [clojure-scraps.data.apigetter :as apigetter]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.data.csv :as csv]
            [clojure.tools.logging :as log])
  (:import [java.time ZoneId ZonedDateTime Duration]
           (java.time.format DateTimeFormatter)
           (org.ta4j.core BaseBarSeriesBuilder)))

(def train-split-percentage 0.7)

(def forex-filename  "data/forex/eurusd-3month-1h.csv")
(def forex-test-filename  "data/forex/eurusd-45days-1h-test.csv")
(def forex-filenames-map {:train-file forex-filename :test-file forex-test-filename})
(def bist-stock-filenames (map (fn [filename] (str "data/bist/" filename))
                               (filter (fn [file] (str/ends-with? file ".csv"))
                                       (str/split (:out (sh/sh "ls" "data/bist")) #"\n"))))

(defn get-filename-from-stock
  "Gets the filename that contains CSV data for given stock. Prepend an F to the stock name for VIOP data."
  [stock]
  (first (filter (fn [filename] (str/includes? filename (str "_" stock))) bist-stock-filenames)))
(def bist-filenames-map (let [filename (get-filename-from-stock "SAHOL")]
                          {:train-file filename :test-file filename}))

(def evolution-filenames-map bist-filenames-map)

(defn csv-data->maps
  [csv-data]
  (mapv zipmap
        (->> (first csv-data) ;; First row is the header
             (map keyword) ;; Drop if you want string keys instead
             repeat)
        (rest csv-data)))

(defn parse-csv-line
  [entry]
  (let [{:keys [open high low close volume] :or {volume "0"}} entry]
    (assoc entry
           :open (Double/parseDouble open)
           :high (Double/parseDouble high)
           :low (Double/parseDouble low)
           :close (Double/parseDouble close)
           :volume (Double/parseDouble volume))))

(defn read-csv-file
  [filename]
  (with-open [reader (io/reader filename)]
    (->> (csv/read-csv reader {:separator \;})
         csv-data->maps
         (map parse-csv-line))))

(defn parse-datetime-inday
  "HELPER: Parses given datetime string in format yyyy-MM-dd HH:mm:ss."
  [dt]
  (ZonedDateTime/parse dt (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss") (ZoneId/of "UTC"))))

(defn candles-to-bar-map
  "Transforms the candle data received into a general format used by datagetter to generate bars."
  [candles]
  (map (fn [candle] (dissoc (assoc candle :datetime (str/replace (:date candle) #"T" " ")) :date)) candles))

(defn get-bars-from-api
  ([]
   (get-bars-from-api 20))

  ([data-count]
   (let [data (reverse (apigetter/get-time-series data-count))
         s (.build (BaseBarSeriesBuilder.))]
     (doseq [{:keys [datetime open high low close volume]} data]
       (.addBar s (parse-datetime-inday datetime) open high low close volume))
     s)))

(defn get-bars
  "Bars should be a sequence of maps containing :open/:high/:low/:close/:volume/:datetime OR :date and :time"
  [bars]
  (let [s (.build (BaseBarSeriesBuilder.))]
    (doseq [{:keys [datetime open high low close volume date time]} bars]
      (if (nil? date)
        (.addBar s (Duration/ofHours 1) (parse-datetime-inday datetime) open high low close volume)
        (.addBar s (Duration/ofHours 1) (parse-datetime-inday (str date " " time)) open high low close volume)))
    s))

(defn- get-bars-from-csv
  "Reads the given file, and generates a bar series from the CSV data."
  [file]
  (-> file
      read-csv-file
      get-bars))

(defn get-bars-for-genetic
  "Reads the experiment dataset and returns it as a ta4j BarSeries."
  [filenames mode]
  (let [training-file (:train-file filenames)
        test-file (:test-file filenames)]
    (if (not= training-file test-file)
      (condp = mode
        :train (get-bars-from-csv training-file)
        :test (get-bars-from-csv test-file)
        (log/warn "unsupported mode for get-bars-for-genetic"))
      (let [bars (get-bars-from-csv (:train-file filenames))
            num-of-bars (.getBarCount bars)
            split-index (int (* train-split-percentage num-of-bars))]
        (condp = mode
          :train (.getSubSeries bars 0 split-index)
          :test (.getSubSeries bars split-index (dec num-of-bars))
          (log/warn "unsupported mode for get-bars-for-genetic"))))))

(defn get-bar-value
  "Returns the bar value of the given bar."
  [bar]
  (-> bar
      .getClosePrice
      .doubleValue))

(defn get-bar-value-at-index
  "Returns the bar value on the given index."
  [bars index]
  (-> bars
      (.getBar index)
      get-bar-value))

(defn get-bar-close-time-at-index
  "Returns the bar end time on the given index."
  [bars index]
  (let [bar (.getBar bars index)] (-> bar .getEndTime .toString str)))

