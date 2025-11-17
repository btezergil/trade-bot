(ns clojure-scraps.params)

(def params
  {:window-range 5
   :crossover-probability 0.5
   :crossover-propagation-probability 0.5 ; TODO: this governs how deep/shallow the crossover goes, try different experiments with this
   :mutation-probability 0.2
   :flip-mutation-probability 0.25
   :population-size 100
   :rank-selection-offset 0.6 ; TODO: UNUSED
   :generation-count 400
   :elitism-ratio 0.05
   :leverage 10
   :capital 1000
   :commission 0.005
   :stoploss-enabled false ; TODO: introduce stoploss and take profit, but later on
   :takeprofit-enabled false
   :stoploss-ratio 0.1
   :takeprofit-ratio 0.2
   :data-window 1000 ; TODO: sliding data and trading windows?
   :trading-window 100
   :trade-threshold 0.05 ; TODO: introduce trade threshold?
   :accuracy-profit-ratio 0.01 ; TODO: what is this even?
   :indicators [:rsi :sma :ema :fisher :fibonacci :engulfing :pinbar] ; TODO: buradaki listeyi bizim dosyadaki listeden populate et
   :prune-height 4 ; indicator count is 2^prune-height
   :default-age 0
   :data-history-window 100
   :fitness-criterion :accuracy-percentage ; must be one of: [:profit, :accuracy, :accuracy-percentage :accuracy-profit-hybrid]
   :fitness-offset 20000
   :max-fitness-scale 25000 ; governs the profit scaling ceiling for hybrid fitness
   :accuracy-factor 0.5 ; governs the weights of accuracy and profit in the hybrid fitness scheme
   })

