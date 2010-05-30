(ns problem211
  (meta {:description "For a positive integer n, let σ2(n) be the sum of the squares of its divisors. For example,

σ2(10) = 1 + 4 + 25 + 100 = 130.
Find the sum of all n, 0 < n < 64,000,000 such that σ2(n) is a perfect square.
"})
  (:use clojure.contrib.repl-utils)
  (:use	[tools.primes :only (divisors#)]
	[clojure.contrib.math :only (expt exact-integer-sqrt)]
	[clojure.test]))

;; to handle 64 million longs, we have to modify the memory requirements
;; (custom-set-variables '(swank-clojure-extra-vm-args '("-server" "-Xmx768M")))

;;(- (expt 2 31 ) 1 )

;; (def highest (divisors 64000000))
;; (type  (reduce + (map #(* % %) highest)))
;; java.lang.Long

;; probably a good idea to use a long-array
;; 5688888803185771
;; (exact-integer-sqrt 5688888803185771)
;; [75424722 114408487]
;; perfect squares can be in a int-array

(defn make-square-divs-transient [limit]
  (loop [i 0 d 2 divs (transient (vec (repeat limit 1)))]
    (if (= d limit) 
      (persistent! divs)
      (if (>= i limit) 
	(recur 0 (inc d) divs)
	(recur (+ i d) d (assoc! divs i (+ (divs i) (* d d))))))))

;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-transient 1000000))))
;; "Elapsed time: 16447.10586 msecs"
;; 7857957051233
;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-transient 2000000))))
;; "Elapsed time: 92184.425185 msecs"
;; 21133820489733

(defn make-square-divs-seq [limit]
  (loop [i 0, d 2, divs (long-array limit 1)]
    (if (= d limit) 
      divs
      (if (>= i limit) 
	(recur 0 (+ d 1) divs)
	(recur (+ i d) d (do (aset divs i (+ (aget divs i) (* d d))) divs))))))

;  (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-seq 100000)))
;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-seq 1000000))))
;; "Elapsed time: 10090.140854 msecs"
;; 7857957051233
;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-seq 2000000))))
;; "Elapsed time: 21896.321766 msecs"
;; 21133820489733
;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-seq 3000000))))
;; "Elapsed time: 33802.737233 msecs"
;; 61677816054533

(set! *warn-on-reflection* true)

(take 12 (map (fn [s] (reduce + (map #(* % %) s))) (map #(divisors %) (iterate inc 1))))

;; memory efficient, but makes too many calculations (finding divisors is expensive.. ) 
;; furthermore, multiplications are done a lot of times on the same numbers (also done with the other methods)
;; exact-integer-sqrt is not cheap either .. 
(defn reduce-sum [limit]
  (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) 
		     (take limit (map (fn [s] (reduce + (map #(* % %) s))) (map #(divisors %) (iterate inc 1)))))))

