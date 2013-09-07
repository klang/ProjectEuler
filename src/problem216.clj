;; Consider numbers t(n) of the form t(n) = 2n^2-1 with n > 1.
;; The first such numbers are 7, 17, 31, 49, 71, 97, 127 and 161.
;; It turns out that only 49 = 7*7 and 161 = 7*23 are not prime.
;; For n < 10000 there are 2202 numbers t(n) that are prime.
;; 
;; How many numbers t(n) are prime for n <= 50,000,000 ?
(ns p216
  (:use [tools.pseudo-primes :only (prime?)]
	[tools.primes :only (prime-factors)]))

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

(def t (map #(- (* 2 % %) 1) (iterate inc 1)))
(def t1 (map #(- (* 2 % %) 1) (range 1 10001)))
;; p216> (time (count (filter #(prime? %) t1)))
;; "Elapsed time: 3856.651387 msecs"
(def t1 (map #(- (* 2 % %) 1) (range 1 10001)))

(def t10-6 (map #(- (* 2 % %) 1) (range 1 100001)))
;; p216> (time (count (filter #(prime? %) t10-6)))
;; "Elapsed time: 52301.032838 msecs"
;; 17185
(def tp (map #(- (* 2 % %) 1) (range 10000 50000 )))
;; (= (count (filter #(prime? %) tp)) (- 9175 2202))
;; (= 6973 (- 9175 2202))
;; (time (count (filter #(prime? %) tp)))
;; "Elapsed time: 18285.902963 msecs"

(def t10-7 (map #(- (* 2 % %) 1) (range 1 1000001)))
;; p216> (time (count (filter #(prime? %) t10-7)))
;; "Elapsed time: 758353.657707 msecs"
;; 141444
(def t10-72 (map #(- (* 2 % %) 1) (range 1000001 2000001)))
;; p216> (time (count (filter #(prime? %) t10-72)))
;; "Elapsed time: 846266.642135 msecs"
;; 127231
(def t10-73 (map #(- (* 2 % %) 1) (range 2000001 3000001)))
(comment (time (count (filter #(prime? %) t10-73))))

(def t10-74 (map #(- (* 2 % %) 1) (range 3000001 4000001)))
; on iMac
;;user=> (time (count (filter #(prime? %) t10-74)))
;;"Elapsed time: 440166.764 msecs"
;;119917
;;user=> (def t10-75 (map #(- (* 2 % %) 1) (range 4000000 10000001)))
;;user=> 
;;user=> (time (count (filter #(prime? %) t10-75)))    

;; 0-1    1-2    2-3 3-4   4-10
(+ 141444 127231 x   119917 )


;(def t (map #(- (* 2 % %) 1) (range 1 50000000)))

;(time (count (filter #(prime? %) t)))
4 999 999 999 999 999
5 000 000 000 000 000

;; limit                number of primes
10,000,000,000,000,000	279,238,341,033,925
