(ns clojure-scraps.data.apigetter
  (:require [envvar.core :as envvar :refer [env]]
            [clj-http.client :as client]
            [cheshire.core :as cheshire]))

(def team-query-params {:symbol "TEAM", :interval "1min", :exchange "NASDAQ"})
(def quote-url "https://api.twelvedata.com/quote")
(def time-series-url "https://api.twelvedata.com/time_series")
(def alphavantage-query-url "https://www.alphavantage.co/query")
(def default-interval 3000)
; TODO: API data casting to bars with duration is not working, find out what the problem is

(defn get-quote
  "Queries the API for data"
  []
  (let [{:keys [body]} (client/get quote-url {:query-params
                                              {"symbol" (:symbol team-query-params)
                                               "interval" (:interval team-query-params)
                                               "exchange" (:exchange team-query-params)
                                               "apikey" (:twelvedata-apikey @env)}})]
    (cheshire/parse-string body true)))

(defn get-forex-time-series
  "Queries the API for data"
  [filename]
  (let [response (client/get time-series-url {:query-params {"symbol" "EUR/USD"
                                                             "interval" "1h"
                                                             "outputsize" (* 24 45)
                                                             "format" "CSV"
                                                             "apikey" (:twelvedata-apikey @env)}})
        body (:body response)]
    (spit filename body)))

(defn get-time-series
  "Queries the API for data"
  ([size] (get-time-series size (:symbol team-query-params)))
  ([size symbol]
   (let [response (client/get time-series-url {:query-params
                                               {"symbol" symbol
                                                "interval" (:interval team-query-params)
                                                "outputsize" size
                                                "apikey" (:twelvedata-apikey @env)}})
         body (cheshire/parse-string (:body response) true)
         values (:values body)]
     values)))
