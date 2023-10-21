(ns clojure-scraps.genetic
  (:require [clojure-scraps.treenode :as node]
            [clojure-scraps.params :as p]))


(defn generate-sequence
  []
  (conj (node/generate-tree)
        (node/generate-tree)))

(vector? (generate-sequence))
