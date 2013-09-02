(ns problem204
  (meta {:description "A Hamming number is a positive number which has no prime factor larger than 5.
So the first few Hamming numbers are 1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15.
There are 1105 Hamming numbers not exceeding 10^8.

We will call a positive number a generalised Hamming number of type n, if it has no prime factor larger than n.
Hence the Hamming numbers are the generalised Hamming numbers of type 5.

How many generalised Hamming numbers of type 100 are there which don't exceed 10^9?"})
  (:use [tools.primes :only (primes)]))

;; implementation from RosettaCode.com
;; http://rosettacode.org/wiki/Hamming_numbers#Clojure

;; Looks a lot like the description found in 
;; Richard Bird & Philip Wadler, Introduction to Functional Programming
;; 7.6.3 The Hamming problem,pp.188-189
;; The generalized Hamming problem is just exercise 7.6.5

(defn smerge [xs ys]
  (lazy-seq
    (let [x (first xs),
          y (first ys),
          [z xs* ys*]
          (cond
            (< x y) [x (rest xs) ys]
            (> x y) [y xs (rest ys)]
            :else   [x (rest xs) (rest ys)])]
      (cons z (smerge xs* ys*)))))
 
(defn smerge3 [xs ys zs]
  (smerge xs (smerge ys zs)))
 
(defn map*n [n ks] (map #(* n %) ks))
 
(def hamming
  (lazy-seq
    (cons 1 (smerge3 (map*n 2 hamming) (map*n 3 hamming) (map*n 5 hamming)))))

;; problem204> (time (count (take-while #(<= % 100000000) hamming)))
;; "Elapsed time: 3.727012 msecs"
;; 1105

(def hamming
  (lazy-seq
    (cons 1 (smerge (map*n 2 hamming) (smerge (map*n 3 hamming) (map*n 5 hamming))))))

(def hamming (lazy-seq (cons 1 (reduce smerge (map #(map*n % hamming) (take-while #(<= % 5) primes))))))

;; problem204> (time (count (take-while #(<= % 100000000) hamming)))
;; "Elapsed time: 15.792796 msecs"
;; 1105

(def hamming (lazy-seq (cons 1 (reduce smerge (map #(map*n % hamming) (take-while #(<= % 100) primes))))))

;; problem204> (time (count (take-while #(<= % 100000000) hamming)))
;; "Elapsed time: 30910.893474 msecs"
;; 924573

;; (time (count (take-while #(<= % 1000000000) hamming)))
;; "Elapsed time: 305696.680426 msecs"
;; 2944730

(defn problem204 [] (count (take-while #(<= % 1000000000) hamming)))
