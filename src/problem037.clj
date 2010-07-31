(ns problem037
  (meta {:description "
The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes."})
  (:use [tools.primes :only (prime-factors)]
	tools.numbers
	[clojure.contrib.lazy-seqs :only (primes)]))

(defn prime? [n]
  (if (>= 1 n)
    false
    (= 1 (count (prime-factors n)))))

(def prime? (memoize prime?))

(defn truncable-prime? [number]
  (loop [ds (rest (digits number)) 
	 sd (butlast (digits number))
	 checks (sorted-set)
	 ]
    (if (= '() ds) 
      (every? prime? checks)
      (let [ids (integer ds)
	    isd (integer sd)] 
	(if (and (< 1 ids)
		 (< 1 isd))
	  (recur (rest ds) (butlast sd) (conj checks ids isd))
	  false)))))

(defn truncable-primes [n]
    (drop 4 (take (+ n 4) (filter #(truncable-prime? %) primes))))

;; user> (trunc-primes 11)
;; (23 37 53 73 313 317 373 797 3137 3797 739397)
;; user> (time (reduce + (truncable-primes 11)))
;; "Elapsed time: 23647.350365 msecs"
;; 748317
(defn problem037 []
  (reduce + (truncable-primes 11)))