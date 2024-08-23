(ns clojure-scraps.treenode 
  (:require [clojure.tools.logging :as log]
            [clojure-scraps.aws :as aws-helper]
            [clojure-scraps.datagetter :as datagetter]
            [clojure.spec.alpha :as s]
            [clojure-scraps.indicators.pinbar :as pinbar]))

(def operators [:and :or])
(def prune-height 3)
(def indicator-count 5)
(def operands (range indicator-count))

(defn generate-operator
  "Generates a random operator, taken from the operators list."
  []
  (rand-nth operators))

(defn generate-operand
  "Generates a random operand, taken from the operands list."
  []
  (rand-nth operands))

; treenode is a three element list representing a tree node and its children
; with this structure, we can get the left child with first, right child with last, and the node itself with second.
(defn generate-node
  [height-remaining]
  (if (> height-remaining 0)
    [(generate-node (dec height-remaining)) (generate-operator) (generate-node (dec height-remaining))]
    [(generate-operand) (generate-operator) (generate-operand)]))

(generate-node 3)

(defn generate-tree
  ([] (generate-node prune-height))
  ([height] (generate-node height)))

(defn mutation
  [node-type node]
  (condp = node-type
    :operator (if (= :and node) :or :and)
    :operand (generate-operand)))

(defn swap-mutation 
  [node]
  (if (vector? node)
    (let [prob (rand)
          left (first node)
          mid (second node)
          right (last node)]
      (cond (< prob 0.2) [left (mutation :operator mid) right] 
            (< prob 0.6) [(swap-mutation left) mid right]
            :else [left mid (swap-mutation right)]))
    (mutation :operand node)))


(rand)
(vector? [2 :and 3])
(mutation :operator :and)
(swap-mutation (generate-tree))
