(ns problem010
  (meta {:description "The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million."})
  (:use	[clojure.contrib.lazy-seqs :only (primes)]
	[clojure.test]))

(defn- solve010 [n]
  (reduce + (take-while #(< % n) primes)))

(deftest test-problem010
  (is (= 17 (solve010 10))))

; problem010> (time (solve010 2000000))
; "Elapsed time: 46393.526973 msecs"
; 142913828922

; (run-tests)

(defn problem010 [] (solve010 2000000))