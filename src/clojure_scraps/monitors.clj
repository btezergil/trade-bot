(ns clojure-scraps.monitors
  (:require [clojure-scraps.dynamo :as dyn]
            [clojure-scraps.params :as p]
            [clojure.spec.alpha :as s]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io]))

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
  (log/info "Writing individuals to table")
  (dorun (pmap (partial dyn/write-strategy-to-table evolution-id) population)))

(defn write-transactions-to-table-monitor
  "Monitor function for evolution that writes every individual of population to the table"
  [transaction-calculator-fn population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (log/info "Writing transactions to table")
  (dorun (pmap (fn [ind] (dorun (pmap (partial dyn/write-transaction-to-table (:guid ind)) (transaction-calculator-fn ind)))) population)))

(defn- write-to-file
  [filename data]
  (spit filename data :append true))

; TODO: develop the functions that write to file instead of DB in order to see whether it's faster or not
(defn write-individuals-to-file-monitor
  "Monitor function for evolution that writes every individual of population to a file to be written to DB later"
  [evolution-id population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (map #(write-to-file (str evolution-id "-individuals.txt") (str % "\n")) population))

(defn write-transactions-to-file-monitor
  "Monitor function for evolution that writes every transaction of population to a file to be written to DB later"
  [evolution-id transaction-calculator-fn population current-generation]
  {:pre [(s/conform :genetic/individual population)]}
  (map #(write-to-file (str evolution-id "-transactions.txt") (-> %
                                                                  transaction-calculator-fn
                                                                  (str "\n")))
       population))

(defn read-individuals-from-file
  "Reads the individual file written by monitor function"
  [evolution-id]
  (with-open [rdr (io/reader (str evolution-id "-individuals.txt"))]
    (let [lines (line-seq rdr)]
      (doall (map read-string lines)))))

(defn read-transactions-from-file
  "Reads the transactions file written by monitor function"
  [evolution-id]
  (with-open [rdr (io/reader (str evolution-id "-transactions.txt"))]
    (let [lines (line-seq rdr)]
      (doall (map read-string lines)))))
