(ns problem025
  (meta {:description "What is the first term in the Fibonacci sequence to contain 1000 digits?"})
  (:use 
   [clojure.test :only (deftest is)]
   [tools.numbers :only (fibos digits)]
   [tools.misc :only (indexed)])
  (:require
   [clojure.string :only (split)]
   [clojure.math.numeric-tower :only (expt) :as math]))


(defn fibo-terms []
  (iterate (fn [[c a b]] [(inc c) b (+ a b)]) [1 0 1]))

(defn solve025 [d]
  (first (take 1 (filter #(not (zero? %)) 
			 (map #(if (<= d (count (digits (nth % 2)))) 
				 (first %) 
				 0) 
			      (fibo-terms))))))

;; user> (time (solve025 1000))
;; "Elapsed time: 7902.703557 msecs"
;; 4782

(deftest test-problem025
  (is (= 12 (solve025 3))))

;; (run-tests)

;; faster version
(defn solve025 [d]
  (first (first (take 1 (drop-while #(<= (second %) (math/expt 10 (dec d))) (indexed (fibos)))))))

;; problem025> (time (problem025))
;; "Elapsed time: 730.76045 msecs"
;; 4782

(defn problem025 [] (solve025 1000))
