(ns problem231
  (meta {:description "The binomial coefficient 10C3 = 120.
120 = 2^3 * 3 * 5 = 2 * 2 * 2 * 3 * 5, and 2 + 2 + 2 + 3 + 5 = 14.

So the sum of the terms in the prime factorisation of 10C3 is 14. 

Find the sum of the terms in the prime factorisation of 20000000C15000000."
	 :note "memory to contain the primes up to 20 million is a bit high, set jvm-opts to -Xmx512"})
  (:use [tools.numbers :only (factorial)]
	[tools.primes :only (factors primes-up-to)]
	[clojure.contrib.lazy-seqs :only (primes)]
	[clojure.test :only (deftest is run-tests)]))

;; :jvm-opts ["-Xmx512M"]

;; C(n,k) == nCk
(defn C [n k]
  (/ (factorial n) (* (factorial k) (factorial (- n k)))))

(defn sum-of-prime-factors [n]
  (reduce + (factors n)))

;;[ n! / (k!*(n-k)!) ]

(defn Ccalc [n k]
  (let [num (range n (- n k) -1) denum (range k 1 -1)]
    {:numerator num
     :denumerator denum
     :factors-num (reduce into [] (map factors num))
     :factors-denum (reduce into [] (map factors denum))}))

(comment
  (Ccalc 20 15)
  ;; a part of the numerator and denumerator can be reduced directly (15 to 6)
  {:numerator     (20 19 18 17 16 15 14 13 12 11 10 9 8 7 6),
   :denumerator                  (15 14 13 12 11 10 9 8 7 6 5 4 3 2)}
  )

(defn Cranges [n k]
  {:numerator [(+ (- n k) 1) n]
   :denumerator [1 k]
   :overlap [(+ (- n k) 1) k]
   :num [(+ k 1) n]
   :den [1 (- n k)]})

(defn Ccalc
  "C can be calculated more efficiently, if even more of the equation is reduced"
  [n k]  
  (let [num (range n k -1) denum (range (- n k) 1 -1)]
    {:numerator num
     :denumerator denum
     :factors-num (reduce into [] (map factors num))
     :factors-denum (reduce into [] (map factors denum))}))

;;2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46
;;2   2   2   2    2     2     2     2     2     2     2     2     2     2     2     2     2     2     2     2     2     2     2
;;  3     3     3        3        3        3        3        3        3        3        3        3        3        3        3
;;    x 5          5              5              5              5              5              5              5              5  
;; --- 2 takes part in the factorisation of (+ (quot 46 2) (quot 46 4) (quot 46 8) (quot 46 16) (quot 46 32)) numbers below 46
;;    2       2          2           2           2           2           2           2           2           2           2
;;                                   2                                               2
;;                                                                                   2
;; -----------------------------------------------------------------------------------------------------------------------------
;;        x 7                  7                    7                    7                    7                    7
;; --- 3 takes part in the factorisation of (+ (quot 46 3) (quot 46 9) (quot 46 27) ) numbers below 46
;;              3                          3                          3                          3                          3
;;                                                                    3
;; -----------------------------------------------------------------------------------------------------------------------------
;;            x x  x 11                               11                               11                               11
;;                       x 13                                     13                                     13
;;                             x  x  x 17                                                 17
;;                                         x 19                                                       19
;;                                               x  x  x 23                                                                   23
;;                                                           x  x  x  x  x 29
;;                                                                             x 31
;;                                                                                   x  x  x  x  x 37
;;                                                                                                     x  x  x 41
;;                                                                                                                 x 43
;;                                                                                                                       x  x  x

(defn fastC [n k]
  (let [fc (Ccalc n k)]
    (merge-with - (frequencies (fc :factors-num)) (frequencies (fc :factors-denum)))))

(defn prime-sum [n k]
  (let [freq (fastC n k)]
    (reduce + (map #(* (key %) (val %)) freq))))

(defn count-factors-under
  "returns the number of times a prime is part of the factorisation of a number below max, without factorising all the numbers up to max"
  [max factor]
  (loop [i (* factor factor)
	 factors (quot max factor)]
    (if (> i max)
      factors
      (recur (* i factor) (+ factors (quot max i))))))

(defn prime-sum-fast [n k q]
  (- (count-factors-under n q) (+ (count-factors-under k q) (count-factors-under (- n k) q))))

(time (def p20 (primes-up-to 20000000)))
;; Elapsed time: 5798.47659 msecs

(deftest theory
  (is (= (prime-sum 2000 1500)
	 (reduce + (map #(* % (prime-sum-fast 2000 1500 %)) (take-while #(< % 2000) p20)))))
  (is (= (prime-sum 20000 15000)
	 (reduce + (map #(* % (prime-sum-fast 20000 15000 %)) (take-while #(< % 20000) p20)))))  
  )

(defn problem231 []
  (reduce + (map #(* % (prime-sum-fast 20000000 15000000 %)) (take-while #(< % 20000000) p20))))
;;(time (problem231))
;;"Elapsed time: 5021.874612 msecs"
;;7526965179680


;; 20000000! / (15000000! * 5000000!)

;; (/ (* 10 9 8) (* 3 2) )


;; (/ (reduce * (range 20000000 15000000 -1)) (reduce * (range 5000000 1 -1)))

;; basically, we need to find the factors of the numbers last 5 million numbers up to 20 million,
;; and reduce the equation with the factors of the first 5 million numbers

;; if we can go through both ranges at the same time, reducing the factors as we go, we have a chance.
;; on the other hand, there are a lot of numbers we don't even have to look at.
;; all the primes in the denominator can be reduced out with the the first multiplum in the numerator

;; 19999996 ... 14999997      2*2*4999999 ...3*4999999     2*2*1 ... 3*4999999
;; ---------------------  =>  ------------------------  => -------------------- 
;;  4999999 ...                  4999999                      1






