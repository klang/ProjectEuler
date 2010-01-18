; The prime factors of 13195 are 5, 7, 13 and 29.

; What is the largest prime factor of the number 600851475143 ?

(use 'clojure.contrib.test-is 'clojure.contrib.lazy-seqs)

;; what is the difference between rem and mod?
;; is one faster than the other or is it just taste?
;; how about quot and /, when using intergers?

(defn factors [n]
  (loop [factors []
	 p primes
	 number n]
    (if (= number 1)
      factors
      (let [f (first p)]
	(if (zero? (rem number f))
	  ;; prime f is a factor of number 
	  ;; (f might still be a factor, so keep the same p)
	  (recur (conj factors f) p (quot number f))
	  ;; try with the next prime
	  (recur factors (rest p) number))))))

(deftest test-factors
  (is (= [] (factors 1)))
  (is (= [5 7 13 29] (factors 13195))))

(defn problem003 [n]
  (peek (factors n)))

(deftest test-problem003
  (is (= 29 (problem003 13195))))

;; user> (time (problem003 600851475143))
;; "Elapsed time: 1.393474 msecs"
;; 6857

; (run-tests)
