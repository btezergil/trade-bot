(ns clojure-scraps.treenode 
  (:require [clojure.tools.logging :as log]
            [clojure-scraps.aws :as aws-helper]
            [clojure-scraps.datagetter :as datagetter]
            [clojure.spec.alpha :as s]
            [clojure-scraps.indicators.pinbar :as pinbar]))

(def operators [:and :or])
(def prune-height 2)
(def indicator-count 5)
(def operands (cons :identity (range indicator-count)))

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
(defn generate-tree
  "Generates a tree with given height, recursively until leaves are reached. No parameter method uses prune-height as default."
  ([] (generate-tree prune-height))
  ([height-remaining] (if (> height-remaining 0)
                        [(generate-tree (dec height-remaining)) (generate-operator) (generate-tree (dec height-remaining))]
                        [(generate-operand) (generate-operator) (generate-operand)])))

; TODO: tree yapisi icin genel olarak bir spec yazilabilir, generative testing vs icin de iyi olur

(defn mutation
  "Genetic mutation operation, flips given operator or randomly replaces given operand."
  [node-type node]
  (condp = node-type
    :operator (if (= :and node) :or :and)
    :operand (generate-operand)))

(defn swap-mutation
  "Swap mutation operation is the simpler genetic mutation operator that can act on the tree.
   Basically, one node is selected to undergo mutation, which changes its value to a new random one." 
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

(defn subtree-mutation
  "Subtree mutation operation is the more complex genetic mutation operator for the node structure.
   It selects a node randomly and completely changes the tree on that node. 
   While swap mutation acts on a node only for operators, subtree mutation changes the whole subtree.
   If a leaf node is reached, randomly replaces the operand."
   [node height]
   (if (vector? node) 
     (let [propagation-probability (rand)
           node-probability (rand)
           left (first node)
           mid (second node)
           right (last node)]
       (cond (< propagation-probability 0.5) (if (< node-probability 0.5)
                                               [(subtree-mutation left (dec height)) mid right]
                                               [left mid (subtree-mutation right (dec height))]) 
             :else (if (< node-probability 0.5)
                     [(generate-tree height) mid right]
                     [left mid (generate-tree height)])))
     (mutation :operand node)))

(defn crossover
  "Genetic crossover operator for the node structure, swaps two branches of different trees."
  [node1 node2]
  (if (vector? node1) 
    (let [propagation-probability (rand)
          node-probability (rand)
          left1 (first node1)
          mid1 (second node1)
          right1 (last node1)
          left2 (first node2)
          mid2 (second node2)
          right2 (last node2)]
      (cond (< propagation-probability 0.5) (if (< node-probability 0.5)
                                              (let [crossover-result (crossover left1 left2)
                                                    new-left1 (first crossover-result)
                                                    new-left2 (second crossover-result)]
                                                [[new-left1 mid1 right1] [new-left2 mid2 right2]])
                                              (let [crossover-result (crossover right1 right2)
                                                    new-right1 (first crossover-result)
                                                    new-right2 (second crossover-result)]
                                                [[left1 mid1 new-right1] [left2 mid2 new-right2]])) 
            :else (if (< node-probability 0.5)
                    [[left2 mid1 right1] [left1 mid2 right2]]
                    [[left1 mid1 right2] [left2 mid2 right1]])))
    [node2 node1]))

(defn check-signal-for-operand
  "Signal check for operand, if a leaf node is found, checks whether the given signal map contains a signal that fits with the tree type at hand."
  [operand signals tree-type]
  (if (= tree-type ((-> operand
                        str
                        keyword) signals))
         tree-type
         :no-signal))

(defn signal-check
  "Node is a eval-tree, signals is a map containing signal for every indicator, tree-type is :long or :short depending on tree direction"
  [node signals tree-type]
  (if (vector? node)
    (let [left-signal (signal-check (first node) signals tree-type)
          right-signal (signal-check (last node) signals tree-type)
          operator (second node)]
      (cond (= :identity left-signal) right-signal
            (= :identity right-signal) left-signal
            (= :and operator) (if (= left-signal right-signal tree-type)
                                tree-type
                                :no-signal)
            (= :or operator) (cond (= left-signal right-signal tree-type) tree-type
                                   (= :no-signal left-signal) right-signal
                                   (= :no-signal right-signal) left-signal
                                   :else :no-signal)))
    (if (= :identity node) 
      :identity
      (check-signal-for-operand node signals tree-type))))


(signal-check [0 :and 1] {:0 :long :1 :long} :long)
(keyword (str 1))
(-> 1
    str
    keyword)
(vector? [2 :and 3])
(mutation :operator :and)
(swap-mutation (generate-tree))
