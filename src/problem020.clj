(ns problem020
  (meta {:description "n! means n * (n-1)  ...  3 * 2 * 1

Find the sum of the digits in the number 100!"})
  (:use
   [tools.numbers :only (digits factorial) :as tools] ))

(defn solve020 [n] 
  (reduce + 
	(digits (factorial n))))

;; user> (time (solve020 100))
;; "Elapsed time: 2.700065 msecs"
;; 648

(defn problem020 [] (solve020 100))
