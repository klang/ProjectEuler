(ns problem119
  (meta {:description "The number 512 is interesting because it is equal to the sum of its digits raised to some power: 5 + 1 + 2 = 8, and 8³ = 512. Another example of a number with this property is 614656 = 28⁴.

We shall define an to be the nth term of this sequence and insist that a number must contain at least two digits to have a sum.

You are given that a2 = 512 and a10 = 614656.

Find a30."})
  (:use 
   [tools.numbers :only (digits)]
   [clojure.math.numeric-tower :only (expt)]))

;; just try brute forcing ..
;; adjusting the ranges until at least 30 members are returned
;; the power is adjusted down, after the result is found to
;; reduce the running time
(defn problem119
  []
  (nth (sort (map #(expt (first %) (second %))
		  (for [base (range 7 70)
			power (range 2 15)
			:when (= base (reduce + (digits (expt base power))))]
		    [base power]))) 29))

;; problem119> (time (problem119))
;; "Elapsed time: 318.856 msecs"   -- power (range 2 50)
;; "Elapsed time: 25.218576 msecs" -- power (range 2 15)
;; 248155780267521
