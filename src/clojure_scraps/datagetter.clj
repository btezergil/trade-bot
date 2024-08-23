(ns clojure-scraps.datagetter
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [cheshire.core :as cheshire]
            [clj-http.client :as client]
            [environ.core :refer [env]]
            [clojure-scraps.aws :as aws-helper])
  (:import [java.time ZoneId ZonedDateTime LocalDate]
           (java.time.format DateTimeFormatter)
           (com.fasterxml.jackson.core JsonParseException)
           (org.ta4j.core BaseBarSeriesBuilder)))

(def nasdaq-100-symbols
  ["ADBE" "ADI" "ADSK" "AEP" "ALGN" "AMAT" "AMD" "AMGN" "ANSS" "ASML" "AVGO" "AZN"
   "BIIB" "BKNG" "BKR" "CDNS" "CEG" "CHTR" "CMCSA" "COST" "CPRT" "CRWD" "CSCO" "CSGP"
   "CSX" "CTAS" "CTSH" "DDOG" "DLTR" "DXCM" "EA" "EBAY" "ENPH" "EXC" "FANG" "FAST"
   "FTNT" "GEHC" "GFS" "GOOGL" "HON" "IDXX" "ILMN" "INTC" "INTU" "ISRG" "JD" "KDP"
   "KHC" "KLAC" "LCID" "LRCX" "LULU" "MAR" "MCHP" "MDLZ" "MELI" "META" "MRNA" "MRVL"
   "MSFT" "MU" "NFLX" "NXPI" "ODFL" "ON" "ORLY" "PANW" "PAYX" "PCAR" "PDD" "PEP"
   "PYPL" "QCOM" "ROST" "SGEN" "SIRI" "SNPS" "TEAM" "TMUS" "TTD" "TXN" "VRSK"
   "VRTX" "WBA" "WBD" "WDAY" "XEL" "ZM" "ZS"])
(def team-query-params {:symbol "TEAM"
                        :interval "1min"
                        :exchange "NASDAQ"})
(def quote-url "https://api.twelvedata.com/quote")
(def time-series-url "https://api.twelvedata.com/time_series")

(defn get-quote
  "Queries the API for data"
  []
  (let [{:keys [body]} (client/get quote-url {:query-params {"symbol"   (:symbol team-query-params)
                                                             "interval" (:interval team-query-params)
                                                             "exchange" (:exchange team-query-params)
                                                             "apikey"   (env :twelvedata-apikey)}})]
    (cheshire/parse-string body true)))

(defn get-forex-time-series
  "Queries the API for data"
  []
  (let [response (client/get time-series-url {:query-params {"symbol"     "EUR/USD"
                                                             "interval"   "1h"
                                                             "outputsize" (* 24 90)
                                                             "format"     "CSV"
                                                             "apikey"     (env :twelvedata-apikey)}})
        body (:body response)]
    (spit "eurusd-3month-1h.csv" body)))

(defn get-time-series
  "Queries the API for data"
  ([size] (get-time-series size (:symbol team-query-params)))
  ([size symbol] (let [response (client/get time-series-url {:query-params {"symbol"     symbol
                                                                            "interval"   (:interval team-query-params)
                                                                            "exchange"   (:exchange team-query-params)
                                                                            "outputsize" size
                                                                            "apikey"     (env :twelvedata-apikey)}})
                       body (cheshire/parse-string (:body response) true)
                       values (:values body)]
                   values)))

(defn csv-data->maps [csv-data]
  (mapv zipmap
        (->> (first csv-data) ;; First row is the header
             (map keyword) ;; Drop if you want string keys instead
             repeat)
        (rest csv-data)))

(defn get-data
  "Data accessor function to be called by other files, gets the data and returns it in the reverse order, which can be processed by ta4j."
  [size]
  (reverse (get-time-series size)))

(defn parse-datetime-inday
  "HELPER: Parses given datetime string in format yyyy-MM-dd HH:mm:ss."
  [dt]
  (ZonedDateTime/parse dt (.withZone (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss") (ZoneId/of "UTC"))))

(defn parse-datetime-daily
  "HELPER: Parses given datetime string in format yyyy-MM-dd."
  [dt]
  (ZonedDateTime/of (.atStartOfDay (LocalDate/parse dt (DateTimeFormatter/ofPattern "yyyy-MM-dd"))) (ZoneId/of "UTC")))

(defn parse-csv-line
  [entry]
  (let [{:keys [open high low close]} entry]
    (assoc entry
           :open (Double/parseDouble open)
           :high (Double/parseDouble high)
           :low (Double/parseDouble low)
           :close (Double/parseDouble close)
           :volume 0)))

(defn read-csv-file
  [filename]
  (with-open [reader (io/reader filename)]
    (->> (csv/read-csv reader {:separator \;})
         csv-data->maps
         (map parse-csv-line)
         reverse)))

(defn get-parser
  "Returns the appropriate parser depending of time interval requested."
  []
  (cond
    (str/ends-with? (:interval team-query-params) "min") parse-datetime-inday
    (str/ends-with? (:interval team-query-params) "day") parse-datetime-daily))

(defn get-bars
  "Bars should be a sequence of maps containing :datetime/:open/:high/:low/:close/:volume"
  ([] (get-bars (get-data 1000)))
  ([bars] (let [s (.build (BaseBarSeriesBuilder.))]
            (doseq [{:keys [datetime open high low close volume]} bars]
              (.addBar s ((get-parser) datetime) open high low close volume))
            s)))

(defn get-bars-for-genetic
  "Reads the experiment dataset and returns it as a ta4j BarSeries."
  []
  (-> "eurusd-3month-1h.csv"
      read-csv-file
      get-bars))

(defn get-subseries-from-bar
  "Returns the subseries within the 'bars' with given start and end indices."
  ([start end] (get-subseries-from-bar (get-bars-for-genetic) start end))
  ([bars start end] (.getSubSeries bars start end)))

(defn get-bar-value-at-index
  "Returns the bar value on the given index."
  [bars index]
  (-> bars
      (.getBar index)
      .getClosePrice
      .doubleValue))
