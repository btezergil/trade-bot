(ns clojure-scraps.datagetter
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [cheshire.core :as cheshire]
            [clj-http.client :as client]
            [environ.core :refer [env]]
            [clojure-scraps.aws :as aws-helper])
  (:import (com.fasterxml.jackson.core JsonParseException)))

(def team-query-params {:symbol "TEAM"
                        :interval "15min"
                        :exchange "NASDAQ"})
(def quote-url "https://api.twelvedata.com/quote")
(def time-series-url "https://api.twelvedata.com/time_series")
(def stock-data-path "/Users/btezergil/phd/790/project/data/")

(defn save-unprocessed-symbol
  [symbol]
  (with-open [f (io/writer (str stock-data-path "unreads.txt") :append true)]
    (spit f (str symbol "\n"))))

(defn get-csv-data
  "Queries the API for data"
  [symbol]

  (let [{:keys [body]} (client/get time-series-url {:query-params {"symbol"     symbol
                                                                   "interval"   (:interval team-query-params)
                                                                   "exchange"   (:exchange team-query-params)
                                                                   "outputsize" 5000
                                                                   "format"     "CSV"
                                                                   "delimiter"  ","
                                                                   "apikey"     (env :twelvedata-apikey)}})]
    (try
      (cheshire/parse-string body true)
      (save-unprocessed-symbol symbol)
      (Thread/sleep 65000)
      (catch JsonParseException e body))))

(defn generate-csv-files
  [symbol]
  (with-open [f (io/writer (str stock-data-path symbol))]
    (spit f (get-csv-data symbol))))

(defn get-stocks-from-api
  "Gets all available stocks"
  []

  (let [{:keys [body]} (client/get "https://api.twelvedata.com/stocks" {:query-params {"exchange" "NASDAQ"
                                                                                       "type" "Common Stock"
                                                                                       "apikey" (env :twelvedata-apikey)}})
        data (:data (cheshire/parse-string body true))
        symbols (map :symbol data)]
    (map generate-csv-files symbols)))

(defn get-stocks-from-file
  "If the unreads file is non-empty, populates the stocks and gets CSV data for them. If not, then queries the API and gets CSV data for all stocks"
  []
  (with-open [f (io/reader (str stock-data-path "unreads.txt"))]
    (let [file-content (slurp f)
          stocks (str/split file-content #"\n")]
      (with-open [f (io/writer (str stock-data-path "unreads.txt") :append false)]
        (spit f ""))
      (map generate-csv-files stocks)))
  )

(defn get-quote
  "Queries the API for data"
  []

  (let [{:keys [body]} (client/get quote-url {:query-params {"symbol"   (:symbol team-query-params)
                                                             "interval" (:interval team-query-params)
                                                             "exchange" (:exchange team-query-params)
                                                             "apikey"   (env :twelvedata-apikey)}})]
    (cheshire/parse-string body true)))

(defn get-time-series
  "Queries the API for data"
  [size]

  (let [response (client/get time-series-url {:query-params {"symbol"     (:symbol team-query-params)
                                                             "interval"   (:interval team-query-params)
                                                             "exchange"   (:exchange team-query-params)
                                                             "outputsize" size
                                                             "apikey"     (env :twelvedata-apikey)}})
        body (cheshire/parse-string (:body response) true)
        values (:values body)]
    values))