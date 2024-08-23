(ns clojure-scraps.dynamo
  (:require [clj-uuid :as uuid]
            [clojure-scraps.params :as p]
            [clojure.spec.alpha :as s]
            [taoensso.faraday :as far]
            [envvar.core :as envvar :refer [env]]))

(def faraday-client-opts-live {:access-key (:aws-access-key @env)
                               :secret-key (:aws-secret-key @env)
                               :endpoint (str "http://dynamodb." (:aws-region @env) ".amazonaws.com")})
(def faraday-client-opts-local {:endpoint "http://localhost:8000"})
(def faraday-client-opts faraday-client-opts-local)

(def strategy-table-vars {:table-name :strategy-v1 :table-key :id})
(def transaction-table-vars {:table-name :transaction-v1 :table-key :id})
(def evolution-table-vars {:table-name :evolution-v1 :table-key :id})
(def evolution-stats-table-vars {:table-name :evolution-stats-v1 :table-key :evolution-id})

(defn write-individual-to-table
  "Records the given genetic indivdual to database"
  [evolution-id individual]
  {:pre [s/valid? :genetic/individual individual]}
  (far/put-item faraday-client-opts
                (:table-name strategy-table-vars)
                {:id (:guid individual)
                 :evolution-id evolution-id
                 :age (:age individual)
                 :fitness (:fitness-score individual)
                 :genetic-sequence (far/freeze (:genetic-sequence individual))}))

(defn read-individual-from-table
  "Queries the strategy-v1 table for the individual with the given id"
  [strategy-id]
  (far/get-item faraday-client-opts
                (:table-name strategy-table-vars)
                {(:table-key strategy-table-vars) strategy-id}))

(defn write-transaction-to-table
  "Records the given transaction to database"
  [strategy-id transaction]
  (far/put-item faraday-client-opts
                (:table-name transaction-table-vars)
                {:id (str (uuid/v4))
                 :strategy-id strategy-id
                 :result (:result transaction)
                 :position (:position transaction)
                 :time-range (:time-range transaction)}))

(defn write-evolution-to-table
  "Records the evolution parameters to database with given id"
  [evolution-id]
  (far/put-item faraday-client-opts
                (:table-name evolution-table-vars)
                (assoc p/params
                       :id evolution-id
                       :timestamp (java.lang.System/currentTimeMillis))))

(defn read-evolution-from-table
  "Queries the evolution-v1 table for the evolution with the given id"
  [evolution-id]
  (far/get-item faraday-client-opts
                (:table-name evolution-table-vars)
                {(:table-key evolution-table-vars) evolution-id}))

(defn write-evolution-stats-to-table
  [evolution-id generation-count best-fitness avg-fitness]
  (far/put-item faraday-client-opts
                (:table-name evolution-stats-table-vars)
                {:evolution-id evolution-id
                 :generation-count generation-count
                 :best-fitness best-fitness
                 :avg-fitness avg-fitness}))

(defn read-evolution-stats-from-table
  [evolution-id]
  (far/query faraday-client-opts
             (:table-name evolution-stats-table-vars)
             {:evolution-id [:eq evolution-id]}))
