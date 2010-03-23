(ns p044
  (:use clojure.contrib.math)
  )

(def pentagonals ; (quot (* n (- (* 3 n) 1)) 2)
  (map (fn [n] (quot (- (* 3 n n) n) 2)) (iterate inc 1)))
(defn pentagonal [n] (quot (- (* 3 n n) n) 2))

