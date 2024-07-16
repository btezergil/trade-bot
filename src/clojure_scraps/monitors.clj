(ns clojure-scraps.monitors
  (:require [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.params :as p]
            [clojure.spec.alpha :as s]))

(defn print-average-fitness-of-population
  "Calculates the average fitness of given population"
  [population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (println (format "Average fitness: %.4f" (/ (reduce + (map :fitness-score population)) (:population-size p/params)))))

(defn write-individuals-to-table-monitor
  "Monitor function for evolution that writes every individual of population to the table"
  [evolution-id population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (dorun (map (partial dyn/write-individual-to-table evolution-id) population)))

(defn write-transactions-to-table-monitor
  "Monitor function for evolution that writes every individual of population to the table"
  [transaction-calculator-fn population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (dorun (map (fn [ind] (dorun (map (partial dyn/write-transaction-to-table (:guid ind)) (transaction-calculator-fn ind)))) population)))
