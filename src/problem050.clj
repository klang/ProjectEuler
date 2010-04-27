(ns problem050
  #^{:description "The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?"
     :question "this is an advanced version of problem 31, right?"
     :answer "restricted partitions, yes"
     :info "(count (take-while #(< % 1000000) primes)) ==> 78498"
     :idea "use the old divisors trick, maybe?"
}
  (:use tools.primes)
  (:use clojure.contrib.lazy-seqs))

;; 2 2+3 2+3+5 2+3+5+7 2+3+5+7+11 2+3+5+7+11+13 ...
;;         i-1   i
;;           [i]-[i-1] sum starting from i
;;           

;; add the first element of plimit to every element in sums (none by init)
;; add the second element of plimit to every element from the second
(defn make-prime-sum-seq [plimit limit]
  (loop [i (int 2)			; the current index
	 p (int 2)			; the current prime
	 sums (int-array limit 2)]      ; the sum of the primes up to index
    (if (= p limit) 
      sums
      (if (>= i limit)
	(recur (int 0) (inc p) sums)
	(recur (inc i) p (do (aset sums i (+ (nth plimit p) (aget sums i))) sums))))))

;;(def plimit (take-while #(< % 1000000) primes))
;;(def psums (make-prime-sum-seq plimit 78498))
;; last element of psums equals the sum of those 78498 primes
;; check that this number is within the range of an int. 

;; problem050> (reduce + plimit)
;; 37550402023

;; no

;; problem050> (prime? 37550402023)
;; true
