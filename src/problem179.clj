;; Find the number of integers 1 < n < 10^7, for which n and n + 1 have the same number of positive divisors. 

;; For example, 14 has the positive divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.
;(load "tools")
(ns p179
  (:use	[tools.primes :only (divisors#)]
	[clojure.contrib.math :only (expt)]))

(defn f [limit]
  (loop [n 2, dn (divisors# n), c 0]
    (if (< limit n) 
      c
      (let [nn (inc n)
	    dnn (divisors# nn)
	    c   (if (= dn dnn) (inc c) c)]
;	(do (println {:n n :dn dn :nn nn :dnn dnn :c c}))
	(recur nn dnn c)))))

;; user> (time (f (expt 10 5)))
;; "Elapsed time: 78273.246317 msecs"
;; 10585

;; not very efficient
;; and runs out of memory !?!?
;; Java heap space
;;  [Thrown class java.lang.OutOfMemoryError]

(defn f1 [limit]
  (loop [n 2, dn 2, nn 3, dnn 2, c 0]
    (if (< limit n) 
      c
      (recur nn dnn (+ nn 1) (divisors# (+ nn 1)) (if (= dn dnn) (+ c 1) c)))))


;; two calls to divisors# does not make things better, memory wise
(defn f2 [limit] (for [n (range 2 limit) :when (= (divisors# n) (divisors# (+ n 1)))] n))
;; user> (time (count (f2 (expt 10 4))))
;; "Elapsed time: 6969.99566 msecs"
;; 1119

;; [delete a lot of different failed implementations, kept the few that makes sense]
;; ----------------------------------------------------------------------------

;; the way divisors# is calculated was very inefficient, hence the above times.
;; by changing the function according to the mathematical definition found on 
;; http://mathworld.wolfram.com/Divisor.html term number (5) that says
;; the total number of divisors is given by
;; d(n) = (1+m1)(1+m2)...(1+mk), where n = p1^m1 = p2^m2 * .. * pk^mk is the prime 
;; factorization of n
;; see (ns tools.primes) for the implementation

;; the original optimized idea + the new implementation

;; p179> (time (f1 (expt 10 5)))
;; "Elapsed time: 10575.103723 msecs"
;; 10585

;; still not efficient, but at least it doesn't run out of memeory.
;; .. and I am sure there is no run-away subset calculations on the way...