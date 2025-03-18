(ns clojure-scraps.evaltree
  (:require [clojure-scraps.params :as p]
            [clojure.tools.logging :as log]
            [clojure.spec.alpha :as s]
            [nature.initialization-operators :as io]))

(def operators [:and :or])
(def operands [:identity :rsi :sma :ema :double-sma :double-ema :fisher :cci :stoch :supertrend])
(def candlesticks #{:engulfing :harami :hammer :hanging-man :inverted-hammer :shooting-star})
; TODO: fibonacci tarafi ilginc bir yapiya sahip, onu kullanmak icin ayri deney yapmak lazim, anlayana kadar fibonacci ekleme
; TODO: engulfing ve pinbar aslinda sinyal cikartacak seviyede hazir, ama nasil kullanacagimizdan emin olana kadar eklemeyelim

;; Spec definitions for supported indicators

(s/def :genetic/index int?)
(s/def :genetic/indicator keyword?)
(s/def :genetic/overbought int?)
(s/def :genetic/oversold int?)
(s/def :genetic/window int?)
(s/def :genetic/multiplier double?)
(s/def :genetic/window1
  (s/and int?
         #(> % 4)
         #(< % 21)))
(s/def :genetic/window2
  (s/and int?
         #(> % 39)
         #(< % 81)))
(s/def :genetic/rsi (s/and (s/keys :req-un [:genetic/index :genetic/indicator :genetic/overbought :genetic/oversold :genetic/window])
                           #(= :rsi (:indicator %))))
(s/def :genetic/ma (s/and (s/keys :req-un [:genetic/index :genetic/indicator :genetic/window])
                          (s/or :indicator #(= :sma (:indicator %)) :indicator #(= :ema (:indicator %)))))
(s/def :genetic/double-ma (s/and (s/keys :req-un [:genetic/index :genetic/indicator :genetic/window1 :genetic/window2])
                                 (s/or :indicator #(= :double-sma (:indicator %)) :indicator #(= :double-ema (:indicator %)))))
(s/def :genetic/fisher (s/and (s/keys :req-un [:genetic/index :genetic/indicator :genetic/window])
                              #(= :fisher (:indicator %))))
(s/def :genetic/cci (s/and (s/keys :req-un [:genetic/index :genetic/indicator :genetic/overbought :genetic/oversold :genetic/window])
                           #(= :cci (:indicator %))))
(s/def :genetic/stoch (s/and (s/keys :req-un [:genetic/index :genetic/indicator :genetic/window])
                             #(= :stoch (:indicator %))))
(s/def :genetic/parabolic-sar (s/and (s/keys :req-un [:genetic/index :genetic/indicator])
                                     #(= :parabolic-sar (:indicator %))))
(s/def :genetic/supertrend (s/and (s/keys :req-un [:genetic/index :genetic/indicator :genetic/window :genetic/multiplier])
                                  #(= :supertrend (:indicator %))))
(s/def :genetic/candlestick (s/and (s/keys :req-un [:genetic/index :genetic/indicator])
                                   #(contains? candlesticks (:indicator %))))
; TODO: candlestickler icin spec yaz

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

;; Generation and mutation functions for supported indicators

(defn generate-rsi
  [index]
  {:post [(s/valid? :genetic/rsi %)]}
  {:index index :indicator :rsi :oversold (rand-int-range 15 45) :overbought (rand-int-range 55 85) :window (rand-int-range 8 20)})

(defn mutate-rsi
  [node]
  {:pre [(s/valid? :genetic/rsi node)] :post [(s/valid? :genetic/rsi %)]}
  (let [param-prob (rand)]
    (condp < param-prob
      0.33 (assoc node :oversold (rand-int-range 15 45))
      0.66 (assoc node :overbought (rand-int-range 55 85))
      (assoc node :window (rand-int-range 8 20)))))

(defn generate-sma
  [index]
  {:post [(s/valid? :genetic/ma %)]}
  {:index index :indicator :sma, :window (rand-int-range 10 100)})

(defn generate-double-sma
  [index]
  {:post [(s/valid? :genetic/double-ma %)]}
  {:index index :indicator :double-sma :window1 (rand-int-range 5 20) :window2 (rand-int-range 40 80)})

(defn generate-ema
  [index]
  {:post [(s/valid? :genetic/ma %)]}
  {:index index :indicator :ema :window (rand-int-range 10 100)})

(defn generate-double-ema
  [index]
  {:post [(s/valid? :genetic/double-ma %)]}
  {:index index :indicator :double-ema, :window1 (rand-int-range 5 20) :window2 (rand-int-range 40 80)})

(defn mutate-ma
  [node]
  {:pre [(s/valid? :genetic/ma node)] :post [(s/valid? :genetic/ma %)]}
  (assoc node :window (rand-int-range 10 100)))

(defn mutate-double-ma
  [node]
  {:pre [(s/valid? :genetic/double-ma node)] :post [(s/valid? :genetic/double-ma %)]}
  (let [param-prob (rand)]
    (if (< param-prob 0.5)
      (assoc node :window1 (rand-int-range 5 20))
      (assoc node :window2 (rand-int-range 40 80)))))

(defn generate-fisher
  [index]
  {:post [(s/valid? :genetic/fisher %)]}
  {:index index, :indicator :fisher, :window (rand-int-range 10 20)})

(defn mutate-fisher
  [node]
  {:pre [(s/valid? :genetic/fisher node)] :post [(s/valid? :genetic/fisher %)]}
  (assoc node :window (rand-int-range 10 20)))

(defn generate-cci [index]
  {:post [(s/valid? :genetic/cci %)]}
  {:index index :indicator :cci :oversold (rand-int-range -80 -120) :overbought (rand-int-range 80 120) :window (rand-int-range 12 28)})

(defn mutate-cci
  [node]
  {:pre [(s/valid? :genetic/cci node)] :post [(s/valid? :genetic/cci %)]}
  (let [param-prob (rand)]
    (condp < param-prob
      0.33 (assoc node :oversold (rand-int-range -80 -120))
      0.66 (assoc node :overbought (rand-int-range 80 120))
      (assoc node :window (rand-int-range 12 28)))))

(defn generate-stochastic-oscillator
  [index]
  {:post [(s/valid? :genetic/stoch %)]} {:index index :indicator :stoch :window (rand-int-range 8 20)})

(defn mutate-stochastic-oscillator
  [node]
  {:pre [(s/valid? :genetic/stoch node)] :post [(s/valid? :genetic/stoch %)]}
  (assoc node :window (rand-int-range 8 20)))

(defn generate-parabolic-sar
  [index]
  {:post [(s/valid? :genetic/parabolic-sar %)]}
  {:index index :indicator :parabolic-sar})

(defn generate-supertrend
  [index]
  {:post [(s/valid? :genetic/supertrend %)]}
  {:index index :indicator :supertrend :window (rand-int-range 8 20) :multiplier (double (/ (rand-int-range 20 40) 10))})

(defn mutate-supertrend
  [node]
  {:pre [(s/valid? :genetic/supertrend node)] :post [(s/valid? :genetic/supertrend %)]}
  (let [param-prob (rand)]
    (if (< param-prob 0.5)
      (assoc node :window (rand-int-range 8 20))
      (assoc node :multiplier (double (/ (rand-int-range 20 40) 10))))))

(defn generate-engulfing
  [index]
  {:post [(s/valid? :genetic/candlestick %)]}
  {:index index, :indicator :engulfing})

(defn generate-harami
  [index]
  {:post [(s/valid? :genetic/candlestick %)]}
  {:index index, :indicator :harami})

; TODO: fibonacci is not implemented, pinbars are not working

(defn generate-fibonacci [index] {:index index, :indicator :fibonacci})

(defn mutate-fibonacci [node] node)

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
      :fisher (generate-fisher index)
      :cci (generate-cci index)
      :stoch (generate-stochastic-oscillator index)
      :parabolic-sar (generate-parabolic-sar index)
      :supertrend (generate-supertrend index)
      :fibonacci (generate-fibonacci index) ; NOT ADDED YET
      :engulfing (generate-engulfing index)
      :harami (generate-harami index)
      :pinbar (generate-pinbar index) ; NOT ADDED YET
      :identity (generate-identity index))))

(defn get-right-index-for-operand
  "Calculates the operand index for the right side of the tree."
  [height]
  (-> 2
      (Math/pow height)
      int))

; EvalTree is a three element list representing a tree node and its children
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

;; Mutation functions defined for EvalTree

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
                   :cci (mutate-cci node)
                   :stoch (mutate-stochastic-oscillator node)
                   :parabolic-sar (generate-operand (:index node))
                   :supertrend (mutate-supertrend node)
                   :fibonacci (mutate-fibonacci node)
                   :engulfing (generate-operand (:index node))
                   :harami (generate-operand (:index node))
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
  (let [mutation-prob (:mutation-probability p/params)
        ind (first ind-list)]
    (if (< (rand) mutation-prob)
      (let [seqn (:genetic-sequence ind)
            long-node (first seqn)
            short-node (second seqn)]
        (log/debug "mutation node: " seqn)
        (io/build-individual (if (< (rand) 0.5) [(mutate-tree long-node) short-node] [long-node (mutate-tree short-node)]) (:parents ind) (:age ind) fitness-func))
      ind)))

;; Crossover functions defined for EvalTree

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

(defn tree-selector
  "Helper function for crossover to return either the long tree selector (i.e. 'first' function) or the short one."
  [long?]
  (if long? first second))

(defn crossover
  "Genetic crossover operator for individuals, calls the crossover for nodes defined by the genetic sequence within individuals.
  Only performs crossover on the long or the short tree."
  ([fitness-func ind-list] (crossover fitness-func (first ind-list) (second ind-list)))
  ([fitness-func ind1 ind2]
   (let [crossover-prob (:crossover-probability p/params)]
     (if (< (rand) crossover-prob)
       (let [seqn1 (:genetic-sequence ind1)
             seqn2 (:genetic-sequence ind2)
             long? (< (rand) 0.5)
             node1 ((tree-selector long?) seqn1)
             node2 ((tree-selector long?) seqn2)
             crossover-result (node-crossover node1 node2)]
         [(io/build-individual (if long? [(first crossover-result) (second seqn1)] [(first seqn1) (first crossover-result)])
                               [seqn1 seqn2]
                               (:default-age p/params)
                               fitness-func)
          (io/build-individual (if long? [(second crossover-result) (second seqn2)] [(first seqn2) (second crossover-result)])
                               [seqn1 seqn2]
                               (:default-age p/params)
                               fitness-func)])
       [ind1 ind2]))))

;; Signal generation/propagation logic for EvalTree
;; Note that these functions do not calculate indicator signals, they use the already calculated indicator signals

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
  "Node is an eval-tree, signals is a map containing signal for every indicator, tree-type is :long or :short depending on tree direction.
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

