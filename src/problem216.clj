;; Consider numbers t(n) of the form t(n) = 2n^2-1 with n > 1.
;; The first such numbers are 7, 17, 31, 49, 71, 97, 127 and 161.
;; It turns out that only 49 = 7*7 and 161 = 7*23 are not prime.
;; For n  10000 there are 2202 numbers t(n) that are prime.
;; 
;; How many numbers t(n) are prime for n <= 50,000,000 ?
(use '[tools.pseudo-primes :only (prime?)]
     '[tools.primes :only (prime-factors)])

(defn tn [n] (- (* 2 n n) 1))
;; user> (time (count (filter #(prime? (tn %)) (range 1 10001))))
;; "Elapsed time: 4030.317292 msecs"
;; 2202

;; user> (time (count (filter #(prime? (tn %)) (range 1 50001))))
;; "Elapsed time: 22074.458384 msecs"
;; 9175

(defn distribution [limit]
  (loop [n (range 1 (+ limit 1)) tn-prime 0
	 tn-not-prime-p-prime 0 tn-prime-p-prime 0
	 tn-prime-p-not-prime 0 tn-not-prime-p-not-prime 0]
    (if (empty? n)
      (list tn-prime tn-not-prime-p-prime tn-prime-p-prime
        tn-prime-p-not-prime tn-not-prime-p-not-prime)
      (let [a (prime? (tn (first n))) b (prime? (first n)) ]
	(recur (rest n)
	 (+ tn-prime (if a 1 0))
	 (+ tn-not-prime-p-prime (if (and (not a) b) 1 0))
	 (+ tn-prime-p-prime (if (and a b) 1 0))
	 (+ tn-prime-p-not-prime (if (and a (not b) ) 1 0))
	 (+ tn-not-prime-p-not-prime (if (and (not a) (not b)) 1 0)))))))
;; user> (time (distribution 10001))
;; "Elapsed time: 4953.147123 msecs"
;; (2202 978 251 1951 6821)

;; user> (time (distribution 50001))
;; "Elapsed time: 27738.833783 msecs"
;; (9175 4274 859 8316 36552)

;; user> (tn 50000000)
;; 4999999999999999



