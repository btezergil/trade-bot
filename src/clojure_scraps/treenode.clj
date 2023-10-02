(ns clojure-scraps.treenode 
  (:require [clojure.tools.logging :as log]
            [clojure-scraps.aws :as aws-helper]
            [clojure-scraps.datagetter :as datagetter]
            [clojure.spec.alpha :as s]
            [clojure-scraps.indicators.pinbar :as pinbar]))

(def operators [:and :or])
(def operands [:rsi :sma :ema :fisher])
(def prune-height 3)

(defn generate-operator
  "Generates a random operator, taken from the operators list."
  []
  (rand-nth operators))

(defn generate-operand
  "Generates a random operand, taken from the operands list."
  []
  (rand-nth operands))

(defmulti generate-node (fn [height-remaining] 
                          (if (> height-remaining 0) (dec height-remaining) 0)))

; treenode is a three element list representing a tree node and its children
; with this structure, we can get the left child with first, right child with last, and the node itself with second.
(defmethod generate-node 0 [_]
  [(generate-operand) (generate-operator) (generate-operand)])

(defmethod generate-node :default [param]
  [(generate-node (dec param)) (generate-operator) (generate-node (dec param))])

(generate-node 3)

(defn generate-tree
  ([] (generate-node prune-height))
  ([height] (generate-node height)))

(generate-tree)
