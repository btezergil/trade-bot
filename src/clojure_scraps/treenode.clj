(ns clojure-scraps.treenode
  (:require [clojure.tools.logging :as log]
            [clojure-scraps.params :as p]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]
            [clojure-scraps.indicators.pinbar :as pinbar]
            [nature.core :as n]
            [nature.initialization-operators :as io]))

(def operators [:and :or])
(def operands [:identity :rsi :sma :ema :double-sma :double-ema])
; TODO: fisher indikator parametrelerini anlamadim, onlari anlamak icin birkac calisma yap, anlayana kadar fisher'i ekleme
; TODO: fibonacci tarafi ilginc bir yapiya sahip, onu kullanmak icin ayri deney yapmak lazim, anlayana kadar fibonacci ekleme
; TODO: engulfing ve pinbar aslinda sinyal cikartacak seviyede hazir, ama nasil kullanacagimizdan emin olana kadar eklemeyelim

(s/def :genetic/index int?)
(s/def :genetic/indicator keyword?)
(s/def :genetic/overbought
  (s/and int?
         #(> % 54)
         #(< % 86)))
(s/def :genetic/oversold
  (s/and int?
         #(> % 14)
         #(< % 46)))
(s/def :genetic/window int?)
(s/def :genetic/window1
  (s/and int?
         #(> % 4)
         #(< % 21)))
(s/def :genetic/window2
  (s/and int?
         #(> % 39)
         #(< % 81)))
(s/def :genetic/rsi (s/keys :req-un [:genetic/index :genetic/indicator :genetic/overbought :genetic/oversold :genetic/window]))
(s/def :genetic/ma (s/keys :req-un [:genetic/index :genetic/indicator :genetic/window]))
(s/def :genetic/double-ma (s/keys :req-un [:genetic/index :genetic/indicator :genetic/window1 :genetic/window2]))
; TODO: fisher ve candlestickler icin spec yaz

(s/def :genetic/fitness-score double?)
(s/def :genetic/genetic-sequence map?)
(s/def :genetic/individual (s/keys :req-un [:genetic/fitness-score :genetic/genetic-sequence]))
(s/def :genetic/population (s/coll-of :genetic/individual))

(defn generate-operator "Generates a random operator, taken from the operators list." [] (rand-nth operators))

(defn rand-int-range
  "rand-int with minimum value, inclusive min, inclusive max"
  [min max]
  (-> max
      inc
      (- min)
      rand-int
      (+ min)))

(defn parse-json-values
  "Value parser function for individuals
  Used when an individual is read from table"
  [key value]
  (if (= key :indicator) (keyword value) value))

(defn keywordize-and-or
  "Convert all and's and or's in the :genetic-sequence list into keywords
  Used when an individual is read from table"
  [value]
  (condp = value "and" (keyword value) "or" (keyword value) value))

(defn generate-rsi [index] {:post [(s/valid? :genetic/rsi %)]} {:index index, :indicator :rsi, :oversold (rand-int-range 15 45), :overbought (rand-int-range 55 85), :window (rand-int-range 8 20)})

(defn mutate-rsi
  [node]
  {:pre [(s/valid? :genetic/rsi node)], :post [(s/valid? :genetic/rsi %)]}
  (let [param-prob (rand)] (condp < param-prob 0.33 (assoc node :oversold (rand-int-range 15 45)) 0.66 (assoc node :overbought (rand-int-range 55 85)) (assoc node :window (rand-int-range 8 20)))))

(defn generate-sma [index] {:post [(s/valid? :genetic/ma %)]} {:index index, :indicator :sma, :window (rand-int-range 10 100)})

(defn generate-double-sma [index] {:post [(s/valid? :genetic/double-ma %)]} {:index index, :indicator :double-sma, :window1 (rand-int-range 5 20), :window2 (rand-int-range 40 80)})

(defn generate-ema [index] {:post [(s/valid? :genetic/ma %)]} {:index index, :indicator :ema, :window (rand-int-range 10 100)})

(defn generate-double-ema [index] {:post [(s/valid? :genetic/double-ma %)]} {:index index, :indicator :double-ema, :window1 (rand-int-range 5 20), :window2 (rand-int-range 40 80)})

(defn mutate-ma [node] {:pre [(s/valid? :genetic/ma node)], :post [(s/valid? :genetic/ma %)]} (assoc node :window (rand-int-range 10 100)))

(defn mutate-double-ma
  [node]
  {:pre [(s/valid? :genetic/double-ma node)], :post [(s/valid? :genetic/double-ma %)]}
  (let [param-prob (rand)] (if (< param-prob 0.5) (assoc node :window1 (rand-int-range 5 20)) (assoc node :window2 (rand-int-range 40 80)))))

(defn generate-fisher [index] {:index index, :indicator :fisher, :window (rand-int-range 5 15)})

(defn mutate-fisher [node] node)

(defn generate-fibonacci [index] {:index index, :indicator :fibonacci})

(defn mutate-fibonacci [node] node)

(defn generate-engulfing [index] {:index index, :indicator :engulfing})

(defn generate-pinbar [index] {:index index, :indicator :pinbar})

(defn generate-identity [index] {:index index, :indicator :identity})

(defn generate-operand
  "Generates a random operand, taken from the operands list."
  [index]
  (let [operand (rand-nth operands)]
    (condp = operand
      :sma (generate-sma index)
      :ema (generate-ema index)
      :rsi (generate-rsi index)
      :double-sma (generate-double-sma index)
      :double-ema (generate-double-ema index)
      :fisher (generate-fisher index) ; NOT ADDED YET
      :fibonacci (generate-fibonacci index) ; NOT ADDED YET
      :engulfing (generate-engulfing index)
      :pinbar (generate-pinbar index)
      :identity (generate-identity index))))

; TODO: operand generation icin bir spec yazilabilir

(defn get-right-index-for-operand
  "Calculates the operand index for the right side of the tree."
  [height]
  (-> 2
      (Math/pow height)
      int))

; treenode is a three element list representing a tree node and its children
; with this structure, we can get the left child with first, right child with last, and the node itself with second.
(defn generate-tree
  "Generates a tree with given height, recursively until leaves are reached. No parameter method uses prune-height as default. Also gives indices to operands."
  ([] (generate-tree (:prune-height p/params)))
  ([height-remaining]
   (if (> height-remaining 0)
     [(generate-tree (dec height-remaining) 0) (generate-operator) (generate-tree (dec height-remaining) (get-right-index-for-operand (dec height-remaining)))]
     (generate-operand 0)))
  ([height-remaining initial-index]
   (if (> height-remaining 0)
     [(generate-tree (dec height-remaining) initial-index) (generate-operator) (generate-tree (dec height-remaining) (+ initial-index (get-right-index-for-operand (dec height-remaining))))]
     (generate-operand initial-index))))

(defn perform-mutation
  "Genetic mutation operation, flips given operator or mutates given operand. Operand mutation can either be a change of parameter or replacement with a new one."
  [node-type node]
  (log/debug "perform-mutation node: " node)
  (condp = node-type
    :operator (if (= :and node) :or :and)
    :operand (let [flip-probability (rand)]
               (if (< flip-probability (:flip-mutation-probability p/params))
                 (generate-operand (:index node))
                 (condp = (:indicator node)
                   :sma (mutate-ma node)
                   :ema (mutate-ma node)
                   :rsi (mutate-rsi node)
                   :double-sma (mutate-double-ma node)
                   :double-ema (mutate-double-ma node)
                   :fisher (mutate-fisher node)
                   :fibonacci (mutate-fibonacci node)
                   :engulfing (generate-operand (:index node))
                   :pinbar (generate-operand (:index node))
                   :identity (generate-operand (:index node)))))))

(defn swap-mutation
  "Swap mutation operation is the simpler genetic mutation operator that can act on the tree.
   Basically, one node is selected to undergo mutation, which changes its value to a new random one."
  [node]
  (log/debug "swap node: " node)
  (if (vector? node)
    (let [prob (rand)
          left (first node)
          mid (second node)
          right (last node)]
      (cond (< prob 0.2) [left (perform-mutation :operator mid) right]
            (< prob 0.6) [(swap-mutation left) mid right]
            :else [left mid (swap-mutation right)]))
    (perform-mutation :operand node)))

(defn find-initial-index
  "Returns the initial index of given tree, needed by subtree mutation since it can arbitrarily select any subtree to be replaced."
  [node]
  (if (vector? node) (find-initial-index (first node)) (:index node)))

(defn subtree-mutation
  "Subtree mutation operation is the more complex genetic mutation operator for the node structure.
   It selects a node randomly and completely changes the tree on that node. 
   While swap mutation acts on a node only for operators, subtree mutation changes the whole subtree.
   If a leaf node is reached, randomly replaces the operand."
  ([node] (subtree-mutation node (:prune-height p/params)))
  ([node height]
   (log/debug "subtree node: " node)
   (if (vector? node)
     (let [propagation-probability (rand)
           node-probability (rand)
           left (first node)
           mid (second node)
           right (last node)]
       (cond (< propagation-probability 0.5) (if (< node-probability 0.5) [(subtree-mutation left (dec height)) mid right] [left mid (subtree-mutation right (dec height))])
             :else (if (< node-probability 0.5) [(generate-tree (dec height) (find-initial-index left)) mid right] [left mid (generate-tree (dec height) (find-initial-index right))])))
     (perform-mutation :operand node))))

(defn mutate-tree "Performs mutation on the given tree." [node] (if (< (rand) 0.5) (swap-mutation node) (subtree-mutation node)))

(defn mutation
  "Genetic mutation operator for individials, performs either swap or subtree mutation on the given node defined in the individual.
  Only performs mutation on the long or the short tree."
  [fitness-func ind-list]
  (let [ind (first ind-list)
        seqn (:genetic-sequence ind)
        long-node (first seqn)
        short-node (second seqn)]
    (log/debug "mutation node: " seqn)
    (io/build-individual (if (< (rand) 0.5) (vector (mutate-tree long-node) short-node) (vector long-node (mutate-tree short-node))) (:parents ind) (:age ind) fitness-func)))

(defn node-crossover
  "Genetic crossover operator for the node structure, swaps two branches of different trees."
  [node1 node2]
  (log/debug "before node1: " node1)
  (log/debug "before node2: " node2)
  (if (vector? node1)
    (let [propagation-probability (:crossover-propagation-probability p/params)
          node-probability (rand)
          left1 (first node1)
          mid1 (second node1)
          right1 (last node1)
          left2 (first node2)
          mid2 (second node2)
          right2 (last node2)]
      (cond (< propagation-probability 0.5)
            (if (< node-probability 0.5)
              (let [crossover-result (node-crossover left1 left2) new-left1 (first crossover-result) new-left2 (second crossover-result)] [[new-left1 mid1 right1] [new-left2 mid2 right2]])
              (let [crossover-result (node-crossover right1 right2) new-right1 (first crossover-result) new-right2 (second crossover-result)] [[left1 mid1 new-right1] [left2 mid2 new-right2]]))
            :else (if (< node-probability 0.5) [[left2 mid1 right1] [left1 mid2 right2]] [[left1 mid1 right2] [left2 mid2 right1]])))
    [node2 node1]))

(defn tree-selector "Helper function for crossover to return either the long tree selector (i.e. 'first' function) or the short one." [long?] (if long? first second))

(defn crossover
  "Genetic crossover operator for individuals, calls the crossover for nodes defined by the genetic sequence within individuals.
  Only performs crossover on the long or the short tree."
  ([fitness-func ind-list] (crossover fitness-func (first ind-list) (second ind-list)))
  ([fitness-func ind1 ind2]
   (let [seqn1 (:genetic-sequence ind1)
         seqn2 (:genetic-sequence ind2)
         long? (< (rand) 0.5)
         node1 ((tree-selector long?) seqn1)
         node2 ((tree-selector long?) seqn2)
         crossover-result (node-crossover node1 node2)]
     (vector
      (io/build-individual (if long? (vector (first crossover-result) (second seqn1)) (vector (first seqn1) (first crossover-result)))
                           (vector seqn1 seqn2) ; TODO: parent'i dogru mu setliyoruz bi bakalim
                           (:default-age p/params)
                           fitness-func)
      (io/build-individual (if long? (vector (second crossover-result) (second seqn2)) (vector (first seqn2) (second crossover-result))) (vector seqn1 seqn2) (:default-age p/params) fitness-func)))))

(defn index-to-keyword
  [operand]
  (-> operand
      :index
      str
      keyword))

(defn check-signal-for-operand
  "Performs the signal check for operand, if a leaf node is found, checks whether the given signal map contains a signal that fits with the tree type at hand.
  Note that this method only checks whether the signal for this operand is fired for the tree type or not, it does not actually generate the signals. "
  [operand signals tree-type]
  (if (= tree-type
         (-> operand
             index-to-keyword
             signals))
    tree-type
    :no-signal))

(defn signal-check
  "Node is a eval-tree, signals is a map containing signal for every indicator, tree-type is :long or :short depending on tree direction.
  Expected signal structure is: {:0 :long/short/no-signal}."
  [node signals tree-type]
  (if (vector? node)
    (let [left-signal (signal-check (first node) signals tree-type)
          right-signal (signal-check (last node) signals tree-type)
          operator (second node)]
      (cond (= :identity left-signal) right-signal
            (= :identity right-signal) left-signal
            (= :and operator) (if (= left-signal right-signal tree-type) tree-type :no-signal)
            (= :or operator) (cond (= left-signal right-signal tree-type) tree-type
                                   (= :no-signal left-signal) right-signal
                                   (= :no-signal right-signal) left-signal
                                   :else :no-signal)))
    (if (= :identity node) :identity (check-signal-for-operand node signals tree-type))))

