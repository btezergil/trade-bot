(ns clojure-scraps.dynamo
  (:require [clj-uuid :as uuid]
            [clojure-scraps.params :as p]
            [clojure.spec.alpha :as s]
            [taoensso.faraday :as far]
            [envvar.core :as envvar :refer [env]]
            [clojure.tools.logging :as log]))

(def faraday-client-opts-live {:access-key (:aws-access-key @env)
                               :secret-key (:aws-secret-key @env)
                               :endpoint (str "http://dynamodb." (:aws-region @env) ".amazonaws.com")})
(def faraday-client-opts-local {:endpoint "http://localhost:8000"})
(def faraday-client-opts faraday-client-opts-local)

(def strategy-table-vars {:table-name :strategy-v1 :table-key :id :index "evolution-id-index"})
(def transaction-table-vars {:table-name :transaction-v2 :table-key :id :index "strategy-id-index"})
(def evolution-table-vars {:table-name :evolution-v1 :table-key :id})
(def evolution-stats-table-vars {:table-name :evolution-stats-v1 :table-key :evolution-id})

(def throughput-map {:read 10000 :write 10000})
(defn create-tables
  "Creates all necessary tables with indexes for initial setup."
  []
  (far/create-table faraday-client-opts
                    (:table-name evolution-table-vars)
                    [(:table-key evolution-table-vars) :s]
                    {:throughput throughput-map})
  (far/create-table faraday-client-opts
                    (:table-name evolution-stats-table-vars)
                    [(:table-key evolution-stats-table-vars) :s]
                    {:range-keydef [:generation-count :n]
                     :throughput throughput-map})
  (far/create-table faraday-client-opts
                    (:table-name strategy-table-vars)
                    [(:table-key strategy-table-vars) :s]
                    {:throughput throughput-map
                     :gsindexes [{:name (:index strategy-table-vars)
                                  :hash-keydef [:evolution-id :s]
                                  :range-keydef [:fitness :n]
                                  :throughput throughput-map}]})
  (far/create-table faraday-client-opts
                    (:table-name transaction-table-vars)
                    [(:table-key transaction-table-vars) :s]
                    {:throughput throughput-map
                     :gsindexes [{:name (:index transaction-table-vars)
                                  :hash-keydef [:strategy-id :s]
                                  :range-keydef [:time-range :s]
                                  :throughput throughput-map}]}))

(defn write-strategy-to-table
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

(defn read-strategy-from-table
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

(defn read-strategies-of-evolution
  "Queries the strategy-v1 table for all strategies belonging to evolution-id"
  [evolution-id]
  (far/query faraday-client-opts
             (:table-name strategy-table-vars)
             {:evolution-id [:eq evolution-id]}
             {:index (:index strategy-table-vars)}))

(defn read-transactions-of-strategy
  "Queries the transaction-v2 table for all transactions belonging to strategy-id"
  [strategy-id]
  (far/query faraday-client-opts
             (:table-name transaction-table-vars)
             {:strategy-id [:eq strategy-id]}
             {:index (:index transaction-table-vars)}))

(defn read-transactions-from-table
  "Scans the transaction-v1 table for the evolution with the given id"
  [evolution-id]
  (let [strategy-ids (map :id (read-strategies-of-evolution evolution-id))]
    (flatten (map read-transactions-of-strategy strategy-ids))))

(defn write-evolution-to-table
  "Records the evolution parameters to database with given id"
  [evolution-id filenames]
  (far/put-item faraday-client-opts
                (:table-name evolution-table-vars)
                (assoc p/params
                       :id evolution-id
                       :timestamp (java.lang.System/currentTimeMillis)
                       :train-file (:train-file filenames)
                       :test-file (:test-file filenames))))

(defn write-evolution-final-stats-to-table
  "Records the final statistics of the evolution"
  [evolution-id stats-map]
  (far/update-item faraday-client-opts
                   (:table-name evolution-table-vars)
                   {:id evolution-id}
                   {:update-expr "SET #name = :value"
                    :expr-attr-names {"#name" "final-stats"}
                    :expr-attr-vals {":value" (far/freeze stats-map)}
                    :return :all-new}))

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

(defn delete-transaction-from-table
  [id]
  (far/delete-item faraday-client-opts
                   (:table-name transaction-table-vars)
                   {(:table-key transaction-table-vars) id}))

(defn delete-strategy-from-table
  [id]
  (far/delete-item faraday-client-opts
                   (:table-name strategy-table-vars)
                   {(:table-key strategy-table-vars) id}))

(defn cleanup-evolution-data
  "Deletes all data related to this evolution-id on all tables"
  [evolution-id]
  (let [strategy-ids (map :id (read-strategies-of-evolution evolution-id))
        transaction-ids (->> strategy-ids
                             (map read-transactions-of-strategy)
                             (flatten)
                             (map :id))]
    (dorun (map delete-transaction-from-table transaction-ids))
    (dorun (map delete-strategy-from-table strategy-ids))
    (dorun (map (fn [entry] (far/delete-item faraday-client-opts
                                             (:table-name evolution-stats-table-vars)
                                             {(:table-key evolution-stats-table-vars) evolution-id
                                              :generation-count (:generation-count entry)})) (read-evolution-stats-from-table evolution-id)))
    (far/delete-item faraday-client-opts (:table-name evolution-table-vars) {(:table-key evolution-table-vars) evolution-id})))

;;(cleanup-evolution-data "33dd1cd5-14d8-4cab-861d-cb675b6ed194")
