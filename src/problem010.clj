; The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

; Find the sum of all the primes below two million.

(use 'clojure.contrib.test-is 'clojure.contrib.lazy-seqs)

(defn problem010 [n]
  (reduce + (take-while #(< % n) primes)))

(deftest test-problem010
  (is (= 17 (problem010 10))))

; user> (time (problem010 2000000))

; (run-tests)
