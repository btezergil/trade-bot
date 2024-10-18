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
  (log/info "writing transaction to table")
  (time (far/put-item faraday-client-opts
                      (:table-name transaction-table-vars)
                      {:id (str (uuid/v4))
                       :strategy-id strategy-id
                       :result (:result transaction)
                       :position (:position transaction)
                       :time-range (:time-range transaction)})))

(defn read-transactions-from-table
  "Scans the transaction-v1 table for the evolution with the given id"
  [evolution-id]
  (let [strategy-ids (map :id (far/query faraday-client-opts
                                         (:table-name strategy-table-vars)
                                         {:evolution-id [:eq evolution-id]}
                                         {:index (:index strategy-table-vars)}))]
    (flatten (map (fn [strategy-id] (far/query faraday-client-opts
                                               (:table-name transaction-table-vars)
                                               {:strategy-id [:eq strategy-id]}
                                               {:index (:index transaction-table-vars)})) strategy-ids))))

;(read-transactions-from-table  "b04084c0-9b9f-4ff8-bf96-cbc6e3d4d75a")
;(far/describe-table faraday-client-opts :transaction-v2)

(defn migrate-transactions
  []
  (loop [i 0]
    (if (< i 500)
      (let [items (far/scan faraday-client-opts
                            (:table-name transaction-table-vars)
                            {:limit 1000})]
        (doall (map (fn [item] (far/put-item faraday-client-opts :transaction-v2 item)
                      (far/delete-item faraday-client-opts :transaction-v1 {:id (:id item)})) items))
        (println "i:" i)
        (recur (inc i))))))
;(migrate-transactions)
(defn write-evolution-to-table
  "Records the evolution parameters to database with given id"
  [evolution-id]
  (far/put-item faraday-client-opts
                (:table-name evolution-table-vars)
                (assoc p/params
                       :id evolution-id
                       :timestamp (java.lang.System/currentTimeMillis))))

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

;(read-evolution-from-table   "b04084c0-9b9f-4ff8-bf96-cbc6e3d4d75a")
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
