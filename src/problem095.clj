;; The proper divisors of a number are all the divisors excluding the number itself. For example, the proper divisors of 28 are 1, 2, 4, 7, and 14. As the sum of these divisors is equal to 28, we call it a perfect number.
;; 
;; Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220, forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.
;; 
;; Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:
;; 
;; 12496  14288  15472  14536  14264 ( 12496  ...)
;; 
;; Since this chain returns to its starting point, it is called an amicable chain.
;; 
;; Find the smallest member of the longest amicable chain with no element exceeding one million.
(load "tools")

(def sum-of-proper-divisors (memoize sum-of-proper-divisors))
(def prime-factors (memoize prime-factors))
;; the sum of the proper divisors of 6 equals 6
;; (= 6 (sum-of-proper-divisors 6))
;; so, 6 belongs to a one element chain.
;; 
;; the sum of the proper divisors of 25 equals 6
;; (= 6 (sum-of-proper-divisors 25))
;; so, 25 will be trapped in an endless loop, unless take care

(defn amicable-chain [number] 
  (loop [p number
	 s (sum-of-proper-divisors number) l 1] ; initial length = the number
    (if (or (= p s) ; number is trapped in endless loop. 25->6->6->6..
	    (<= s 0) 
	    (< 1000000 s))
      false
      (if (= s number)
	(vector number l)
	(recur s (sum-of-proper-divisors s) (inc l)))
      )))
;;  6 ->   6 ->   6 ...
;; 25 ->   6 ->   6 ...
;;562 -> 284 -> 220 -> 284 -> 220 ...


(defn amicable-chain [number] 
  (loop [p number
	 s (sum-of-proper-divisors number) l 1] ; initial length = the number
    
    (if (or (and (< s p) (= l 1)) ;; we will only be looking at chains that inc
	    (= s p)
	    (<= s 0) 
	    (< 1000000 s));; up to a point
      false
      (if (= s number)
	(vector number l)
	(recur s (sum-of-proper-divisors s) (inc l)))
      )))
;; this chain somehow can't be handled..
;;1064 -> 1336 -> 1184 -> 1210 -> 1184 -> 1210 ...



(defn amicable-chain [number] 
  (loop [p number
	 seen #{}
	 s (sum-of-proper-divisors number) l 1] ; initial length = the number
    
    (if (or (and (< s p) (seen s)) ;; we will only be looking at chains that inc
	    (= s p)
	    (<= s 0) 
	    (< 1000000 s));; up to a point
      false
      (if (= s number)
	(vector number l)
	(recur s (conj seen s) (sum-of-proper-divisors s) (inc l)))
      )))
(def amicable-chain (memoize amicable-chain))
;; below 25000.. 35000 ..  45000 and 55000

;; wild guess .. which is correct
;;[14316 28]