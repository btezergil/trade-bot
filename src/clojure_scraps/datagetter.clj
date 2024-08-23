(ns clojure-scraps.datagetter
  (:require [clojure.tools.logging :as log]
            [cheshire.core :as cheshire]
            [clj-http.client :as client]
            [environ.core :refer [env]]
            [clojure-scraps.aws :as aws-helper]))

(def team-query-params {:symbol "TEAM"
                        :interval "15min"
                        :exchange "NASDAQ"})
(def quote-base-url "https://api.twelvedata.com/quote")
(def time-series-base-url "https://api.twelvedata.com/time_series")

(defn get-quote
  "Queries the API for data"
  []

  (let [response (client/get quote-base-url {:query-params {"symbol" (:symbol team-query-params)
                                                            "interval" (:interval team-query-params)
                                                            "exchange" (:exchange team-query-params)
                                                            "apikey" (env :twelvedata-apikey)}})
        body (:body response)]
    (cheshire/parse-string body true)))

(defn get-time-series
  "Queries the API for data"
  [size]

  (let [response (client/get time-series-base-url {:query-params {"symbol" (:symbol team-query-params)
                                                                  "interval" (:interval team-query-params)
                                                                  "exchange" (:exchange team-query-params)
                                                                  "outputsize" size
                                                                  "apikey" (env :twelvedata-apikey)}})
        body (cheshire/parse-string (:body response) true)
        values (:values body)]
    values))