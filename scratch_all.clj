(ns scratch
  (:require
   [tools.primes :only (primes) :as tools]
   [clojure.math.combinatorics :only (cartesian-product) :as comb]
   [clojure.math.numeric-tower :only (exact-integer-sqrt lcm) :as math]))xe

(def p001 ((fn [n] (reduce + (map #(if (or (= 0 (mod % 5)) (= 0 (mod % 3))) % 0) (range n)))) 1000))

(defn fibo [] (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(def p002 ((fn [n] (reduce + (filter #(even? %) (take-while #(< % n) (fibo))))) 4000000))

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

(def p003 ((fn [n] (peek (factors n))) 600851475143))

(defn palindrome? [m]
  (= (apply str (reverse (str m))) (str m)))

(defn n-digit-range [n]
  (range (math/expt 10 (- n 1)) (math/expt 10 n)))

(def p004 ((fn [n]
             (reduce max (for [x (range (math/expt 10 (- n 1)) (math/expt 10 n)) 
                               y (range (math/expt 10 (- n 1)) (+ x 1))] 
                           (if (palindrome? (* x y)) (* x y) 0)))) 3))

(def p010 ((fn [n] (reduce + (take-while #(< % n) tools/primes))) 2000000))


[p001 p002 p003 p004 5 6 7 8 9 p010]
