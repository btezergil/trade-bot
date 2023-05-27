(ns clojure-scraps.datagetter
  (:require [clojure.java.io :as io]
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

  (let [response (client/get time-series-url {:query-params {"symbol"     symbol
                                                             "interval"   (:interval team-query-params)
                                                             "exchange"   (:exchange team-query-params)
                                                             "outputsize" 5000
                                                             "format"     "CSV"
                                                             "delimiter"  ","
                                                             "apikey"     (env :twelvedata-apikey)}})
        body (:body response)]
    (try
      (cheshire/parse-string body true)
      (save-unprocessed-symbol symbol)
      (catch JsonParseException e body))))

(defn generate-csv-files
  [symbol]
  (with-open [f (io/writer (str stock-data-path symbol))]
    (spit f (get-csv-data symbol))))

(defn get-stocks
  "Gets all available stocks"
  []

  (let [response (client/get "https://api.twelvedata.com/stocks" {:query-params {"exchange" "NASDAQ"
                                                                                 "apikey" (env :twelvedata-apikey)}})
        body (:body response)
        data (:data (cheshire/parse-string body true))
        symbols (map :symbol data)]
    (map generate-csv-files symbols)))

(defn get-quote
  "Queries the API for data"
  []

  (let [response (client/get quote-url {:query-params {"symbol"   (:symbol team-query-params)
                                                       "interval" (:interval team-query-params)
                                                       "exchange" (:exchange team-query-params)
                                                       "apikey"   (env :twelvedata-apikey)}})
        body (:body response)]
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