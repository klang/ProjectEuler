(ns problem203
  (:use [tools.primes :only (divisors# prime-factors divisors)]
	[tools.pseudo-primes :only (next-prime prime?)]
	[tools.numbers :only (factorial)]
	[clojure.contrib.math :only (expt exact-integer-sqrt)]
	[clojure.contrib.lazy-seqs :only (primes)]
	[clojure.test]))

;; from http://rosettacode.org/wiki/Pascal's_triangle#Clojure
(defn nextrow [row]
  (vec (concat [1] (map #(apply + %) (partition 2 1 row)) [1] )))
 
(defn pascal [n]
  (assert (and (integer? n) (pos? n)))
  (let [triangle (take n (iterate nextrow [1]))]
    (doseq [row triangle]
      (println row))))

;;problem203> (reduce into #{} (take 8 (iterate nextrow [1])))
;;#{1 2 3 35 4 5 6 7 10 15 20 21}
;;problem203> (count  (reduce into #{} (take 8 (iterate nextrow [1]))))
;;12

(defn distinct-pascal [n]
  (reduce into #{} (take n (iterate nextrow [1]))))

(defn square-free-pascal [n]
  (loop [p primes
	 dp (distinct-pascal n)]
    (if (< (reduce max dp) (expt (first p) 2)) (list (first p) dp)
	(recur (rest p) (filter #(not (zero? (rem % (expt (first p) 2)))) dp)))))

(comment
  (reduce + (square-free-pascal 8)))

;; problem203> (filter prime? (distinct-pascal 51))
;; (2 3 5 37 7 41 11 43 13 47 17 19 23 29 31)

;; problem203> (time (def dp49 (square-free-pascal 49)))
;; "Elapsed time: 187455.591571 msecs"
;; problem203> (difference (set (second dp49)) (set dp48))
;; #{}
;; problem203> (first dp49)
;; 3124519
;; problem203> (time (def dp50 (square-free-pascal 50)))
;; "Elapsed time: 186927.913462 msecs"
;; problem203> (difference (set (second dp50)) (set (second dp49)))
;; #{}
;; problem203> (first dp50)
;; 3124519
;; problem203> (time (def dp51 (square-free-pascal 51)))
;; "Elapsed time: 206650.517801 msecs"
;; #'problem203/dp51
;; problem203> (first dp51)
;; 3124519
;; problem203> (difference (set (second dp51)) (set (second dp50)) )
;; #{}
;; problem203> (reduce + (second dp51))
;; 34029210557338

;;..... there is always a better way ... 

;; C(n,k)
(defn C [n k]
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

;; [daniel.is.fischer] Any prime dividing C(n,k) is ≤ n, so you need only the primes up to 47.

;; [MarleysGhost] I was surprised at how few primes were necessary. By the time even the largest elements of the triangle were divided by primes up through 43, they were reduced to less than 47*47.
;; [quilan] Well, yeah... highest numerator term [ n! / (k!*(n-k)!) ] is going to be 51!, so the highest prime-factor will have to be <= 51. Therefore, 47.

