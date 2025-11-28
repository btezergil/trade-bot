(ns clojure-scraps.monitors
  (:require [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.params :as p]
            [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [envvar.core :as envvar :refer [env]]))

(defn- add-current-generation-info-to-individuals
  [population current-generation]
  (map #(assoc % :current-generation current-generation) population))

(defn print-average-fitness-of-population
  "Calculates the average fitness of given population"
  [population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (log/info (format "Average fitness: %.4f" (double (/ (reduce + (map :fitness-score population)) (:population-size p/params))))))

(defn save-fitnesses-for-current-generation
  "Saves the current generation's best and average fitness into the table
  'gen-count' function parameter is the atom that saves the current generation count"
  [evolution-id gen-count population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (let [fitness-list (map :fitness-score population)
        popsize (:population-size p/params)
        avg-fitness (double (/ (reduce + fitness-list) popsize))
        best-fitness (apply max fitness-list)]
    (dyn/write-evolution-stats-to-table evolution-id @gen-count best-fitness avg-fitness fitness-list)
    (swap! gen-count inc)))

(defn write-individuals-to-table-monitor
  "Monitor function for evolution that writes every individual of population to the table"
  [evolution-id population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (log/info "Writing individuals to table")
  (dorun (pmap (partial dyn/write-strategy-to-table evolution-id) (add-current-generation-info-to-individuals population current-generation))))

(defn write-transactions-to-table-monitor
  "Monitor function for evolution that writes every individual of population to the table"
  [transaction-calculator-fn population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (log/info "Writing transactions to table")
  (dorun (pmap (fn [ind] (dorun (pmap (partial dyn/write-transaction-to-table (:guid ind)) (transaction-calculator-fn ind)))) (add-current-generation-info-to-individuals population current-generation))))

(defn- write-to-file
  [filename data]
  (spit filename (str data "\n") :append true))

(defn- generate-transactions-map
  [strategy-id transactions]
  {:strategy-id strategy-id
   :transactions transactions})

(defn- generate-filename
  [evolution-id file-suffix]
  (if (:in-container @env)
    (str (:out-file-path @env) evolution-id file-suffix)
    (str  evolution-id file-suffix)))

(defn save-fitnesses-to-file-for-current-generation
  "Saves the current generation's best and average fitness into the table
  'gen-count' function parameter is the atom that saves the current generation count"
  [evolution-id population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (let [fitness-list (map :fitness-score population)
        popsize (:population-size p/params)
        avg-fitness (double (/ (reduce + fitness-list) popsize))
        best-fitness (apply max fitness-list)
        stat-map {:evolution-id evolution-id
                  :generation-count current-generation
                  :best-fitness best-fitness
                  :avg-fitness avg-fitness
                  :fitness-list fitness-list}]
    (write-to-file (generate-filename evolution-id "-fitnesses.txt") stat-map)))

(defn write-individuals-to-file-monitor
  "Monitor function for evolution that writes every individual of population to a file to be written to DB later"
  [evolution-id population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (doall (map #(write-to-file (generate-filename evolution-id "-individuals.txt") %) (add-current-generation-info-to-individuals population current-generation))))

(defn write-transactions-to-file-monitor
  "Monitor function for evolution that writes every transaction of population to a file to be written to DB later"
  [evolution-id transaction-calculator-fn population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (doall (map #(write-to-file (generate-filename evolution-id "-transactions.txt") (generate-transactions-map (:guid %) (transaction-calculator-fn %))) (add-current-generation-info-to-individuals population current-generation))))

