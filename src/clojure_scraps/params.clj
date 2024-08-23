(ns clojure-scraps.params)

(def params
  {:indicator-count 5,
   :window-size-min 3,
   :window-size-max 15, ; TODO: window-size'i fix param olarak kullansak da training'e katmasak makul olur mu
   :crossover-probability 0.2,
   :mutation-probability 0.1,
   :flip-mutation-probability 0.25,
   :population-size 2,
   :rank-selection-offset 0.6,
   :generation-count 0,
   :elitism-percentage 0.05,
   :leverage 100,
   :capital 1000,
   :commission 0.005,
   :stoploss-enabled false,
   :takeprofit-enabled false,
   :stoploss-ratio 0.1,
   :takeprofit-ratio 0.2,
   :data-window 1000,
   :trading-window 100,
   :trade-threshold 0.05,
   :accuracy-profit-ratio 0.01,
   :indicators [:rsi :sma :ema :fisher :fibonacci :engulfing :pinbar],
   :prune-height 2,
   :default-age 0})

