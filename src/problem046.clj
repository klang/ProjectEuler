(ns problem046
  (meta {:description "It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.

9  =  7 + 2x1²
15 =  7 + 2x2²
21 =  3 + 2x3²
25 =  7 + 2x3²
27 = 19 + 2x2²
33 = 31 + 2x1²

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?"})
  (:use 
   [tools.primes :only (prime? primes)])
  (:require 
   [clojure.math.combinatorics :only (cartesian-product) :as comb]))

(def twice-a-square (map #(* 2 % %) (iterate inc 1)))
(def odd-composite-numbers (filter #(and (odd? %) (not (prime? %))) (iterate inc 2)))

(defn not-a-prime-and-twice-a-square? [odd-composite-number]
  (let [p (take-while #(< % odd-composite-number) primes)
	s (take-while #(< % (- odd-composite-number 1)) twice-a-square)
	f (take 1 (filter #(= (+ (first % ) (second %)) odd-composite-number)
			  (comb/cartesian-product p (reverse s))))]
    ;{:primes p :twice-a-squares s :found f}
    (zero? (count f))))

;; cartesian-product is a dark horse in this game .. the combinations can be quite big, 
;; ..byy reversing
;; simply take a whild guess about the limit and stop if the calculations take more than a minute

;; .. maybe it is possible to simply make a lazy sequence, delivering the numbers
;; that doesn't follow the "a prime and twice a square" rule

;; (take 1 (filter not-a-prime-and-twice-a-square? (take 2000 odd-composite-numbers)))
;; about 8 seconds
;; ()
;; (take 1 (filter not-a-prime-and-twice-a-square? (take 2000 (drop 2000 odd-composite-numbers))))
;; another 4 seconds
;;5777
;; execution time is acceptable

;; another idea:
;; mark off all primes, and for each prime, mark off the prime + twice-a-square
(defn problem046 []
  (first (take 1 (filter not-a-prime-and-twice-a-square? (take 2000 (drop 2000 odd-composite-numbers))))))
