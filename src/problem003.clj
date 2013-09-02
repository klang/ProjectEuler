(ns problem003
  (meta {:description "The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143 ?"})
  (:use clojure.test)
  (:require [tools.primes :only (primes) :as tools]))

;; what is the difference between rem and mod?
;; is one faster than the other or is it just taste?
;; how about quot and /, when using intergers?

(defn factors [n]
  (loop [factors []
	 p tools/primes
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

(defn- solve003 [n]
  (peek (factors n)))

(deftest test-problem003
  (is (= 29 (solve003 13195))))

;; user> (time (solve003 600851475143))
;; "Elapsed time: 85.231813 msecs"
;; 6857

(defn problem003 [] (solve003 600851475143))

(defn factor [num]
  (loop [n num cur 2]
    (if (= n cur)
      n
      (if (zero? (rem n cur))
	(recur (quot n cur) cur)
	(recur n (inc cur))))))

(defn factors-faster [num]
  (loop [f (transient  []) n num cur 2]
    (if (= n 1)
      (persistent! f)
      (if (zero? (rem n cur))
	(recur (conj! f cur) (quot n cur) cur)
	(recur f n (inc cur))))))

;; version that uses primes is slower, when very big primes are used
;; problem003> (time (factors-faster 600851475141))
;; "Elapsed time: 10021.280163 msecs"
;; [3 11981 16716787]
;; problem003> (time (factors 600851475141))
;; Evaluation aborted. (after quite some time)
