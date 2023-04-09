(ns clojure-scraps.deneme)

(defn addition
  "Add function implementation"
  [sqn]
  (+ (first sqn) (if (empty? (rest sqn)) (+ 0) (addition (rest sqn)))))

(defn append
  "Append an element at the end of a seq"
  [sqn x]
  (cons (first sqn) (if (empty? (rest sqn)) (cons x []) (append (rest sqn) x))))