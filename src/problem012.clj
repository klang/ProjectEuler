(ns problem012
  (meta {:description "What is the value of the first triangle number to have over five hundred divisors?"})
  (:use tools.numbers)
  (:use tools.primes)
  (:use clojure.contrib.combinatorics)
  (:use clojure.set)
  (:use clojure.test))

;; as a function:
(defn triangle [n] (reduce + (range n 0 -1)))

;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55,
;; 1             ; [ 1 2]
;; 1+2           ; [ 3 3]
;; 1+2+3         ; [ 6 4]
;; 1+2+3+4       ; [10 5]
;; 1+2+3+4+5     ; [15 6]
;; 1+2+3+4+5+6   ; [21 7]
;; 1+2+3+4+5+6+7 ; [28 8]  --> 28 is first to have more than 5 divisors

;; as a lazy sequence
(defn triangles []
  (map first (iterate (fn [[a b]] [(+ a b) (inc b)]) [1 2])))

;; naive way to find number of divisors
(comment
  (defn divisors [n]
    (count (set (map #(reduce * %) (subsets (factors n))))))
)

(defn solve012 [n]
  "returns first triangle number that has more than n divisors"
  (first (filter #(> (count (divisors %)) n) (triangles))))

(deftest test-problem012
  (is (= 28 (solve012 5))))

;(run-tests)

;; user> (time (problem012 500))
;; "Elapsed time: 39017.057782 msecs"
;; 76576500
;; user> (divisors 76576500)
;; 576

(defn problem012 [] (solve012 500))

;; idea for a more efficient implementation of divisors?
(comment
  (defn divisors [n]
    "All divisors of n.
Combinations of products of prime factors will give all divisors:
1, (factors n), (subsets (factors n)), n"
    (let [f (factors n)
	  ss (subsets (factors n))])
    (vec (conj (set (factors n)) 1 n)))
)
;; (disj (set (subsets (factors 28))) (factors 28) () '(2) '(7))
;; (factors 28)
;; 1 28



