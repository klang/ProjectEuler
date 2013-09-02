(ns problem007
  (meta {:description "By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

 What is the 10001st prime number?

as we have a lazy sequence for the primes, this problem is really, really easy .. this is almost cheating
counting from zero, so the 10001st will be the 10000th number in the sequence"})
  (:use clojure.test)
  (:require [tools.primes :only (primes) :as tools]))

(defn- solve007 [n]
  (nth tools/primes (- n 1)))

(deftest test-problem007
  (is (= 13 (solve007 6))))

; user> (time (solve007 10001))
; "Elapsed time: 1.61948 msecs"
; 104743

; (run-tests)

(defn problem007 [] (solve007 10001))
