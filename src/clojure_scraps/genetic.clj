(ns clojure-scraps.genetic
  (:require [clojure-scraps.treenode :as node]
            [clojure-scraps.params :as p]))


(defn generate-sequence
  []
  (conj (node/generate-tree)
        (node/generate-tree)))

(defn generate-signals
  "Generates signals on the given data-index"
  [tree data-index direction]
  (if (vector? tree)
    (merge (generate-signals (first tree) data-index direction) (generate-signals (last tree) data-index direction))
    {(node/index-to-keyword tree) :long})) ; TODO: call strategy methods for the operators instead of this placeholder

(merge {:0 :long} {:1 :long})
(vector? (generate-sequence))
(generate-signals (node/generate-tree) 0 :long)
