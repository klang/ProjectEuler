(ns problem010
  (:use	[clojure.contrib.lazy-seqs :only (primes)]
	[clojure.test]))

; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

; Find the sum of all the primes below two million.

(defn problem010 [n]
  (reduce + (take-while #(< % n) primes)))

(deftest test-problem010
  (is (= 17 (problem010 10))))

; problem010> (time (problem010 2000000))
; "Elapsed time: 46393.526973 msecs"
; 142913828922

; (run-tests)
