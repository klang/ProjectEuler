(ns problem020
  (meta {:description "n! means n * (n-1)  ...  3 * 2 * 1

Find the sum of the digits in the number 100!"})
  (:use [clojure.contrib.str-utils2 :only (split)]))

(defn digits [n]
  (map #(. Integer parseInt % 10) 
	     (filter #(not (= % "")) (split (str n) #""))))

(defn bang [n] 
  (reduce * (range n 0 -1)))

(defn problem020 [n] 
  (reduce + 
	(digits (bang n))))

;; user> (time (problem020 100))
;; "Elapsed time: 2.700065 msecs"
;; 648

