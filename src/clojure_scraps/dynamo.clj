(ns clojure-scraps.dynamo
  (:require [clj-uuid :as uuid]
            [environ.core :refer [env]]
            [clojure-scraps.params :as p]
            [clojure.spec.alpha :as s]
            [taoensso.faraday :as far]))

(def faraday-client-opts-live {:access-key (env :aws-access-key) :secret-key (env :aws-secret-key)})
(def faraday-client-opts-local {:endpoint "http://localhost:8000"})
(def faraday-client-opts faraday-client-opts-local)

(def strategy-table-vars {:table-name :strategy-v1 :table-key :id})
(def transaction-table-vars {:table-name :transaction-v1 :table-key :id})
(def evolution-table-vars {:table-name :evolution-v1 :table-key :id})

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
(far/list-tables faraday-client-opts)

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
                (assoc p/params :id evolution-id)))

(defn read-evolution-from-table
  "Queries the evolution-v1 table for the evolution with the given id"
  [evolution-id]
  (far/get-item faraday-client-opts
                (:table-name evolution-table-vars)
                {(:table-key evolution-table-vars) evolution-id}))
