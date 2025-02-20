(ns clojure-scraps.monitors
  (:require [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.params :as p]
            [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]))

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
    (dyn/write-evolution-stats-to-table evolution-id @gen-count best-fitness avg-fitness)
    (swap! gen-count inc)))

(defn write-individuals-to-table-monitor
  "Monitor function for evolution that writes every individual of population to the table"
  [evolution-id population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (dorun (pmap (partial dyn/write-strategy-to-table evolution-id) population)))

(defn write-transactions-to-table-monitor
  "Monitor function for evolution that writes every individual of population to the table"
  [transaction-calculator-fn population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (dorun (pmap (fn [ind] (dorun (pmap (partial dyn/write-transaction-to-table (:guid ind)) (transaction-calculator-fn ind)))) population)))
