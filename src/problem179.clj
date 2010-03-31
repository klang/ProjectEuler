;; Find the number of integers 1 < n < 10^7, for which n and n + 1 have the same number of positive divisors. 

;; For example, 14 has the positive divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.
;(load "tools")
(ns p179
  (:use	[tools.primes :only (divisors#)]
	[clojure.contrib.math :only (expt exact-integer-sqrt)]))

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

;; still not efficient, but at least it doesn't run out of memeory. (64MB)
;; .. and I am sure there is no run-away subset calculations on the way...

;; p179> (time (f1 (expt 10 7)))
;; "Elapsed time: 4343526.044964 msecs"
;; 986262

(def p179limit10
     (let [v (transient (vec (repeat 10 1)))] ;  [1 1 1 1 1 1 1 1 1 1])
       (assoc! v 2 (+ (v 2) 1))		; 2
       (assoc! v 4 (+ (v 4) 1))
       (assoc! v 6 (+ (v 6) 1))
       (assoc! v 8 (+ (v 8) 1))
       (assoc! v 3 (+ (v 3) 1))		; 3
       (assoc! v 6 (+ (v 6) 1))
       (assoc! v 9 (+ (v 9) 1))		; 4
       (assoc! v 4 (+ (v 4) 1))
       (assoc! v 8 (+ (v 8) 1))
       (assoc! v 5 (+ (v 5) 1))		; 5
       (assoc! v 6 (+ (v 6) 1))		; 6
       (assoc! v 7 (+ (v 7) 1))		; 7
       (assoc! v 8 (+ (v 8) 1))		; 8
       (assoc! v 9 (+ (v 9) 1))		; 9
       (persistent! v)))

(def p179limit10print
     (for [i (range 2 10) j (range i 10 i)] 
       (str "(assoc! v "j" (+ (v "j") 1))\n")))

(def p179limit10hard
     (let [v (transient (vec (repeat 10 1)))]
       (doall
	(for [i (range 2 10) j (range i 10 i)] 
	  (assoc! v j (+ (v j) 1))))
       (persistent! v)))

; (println p179limit10print)

(defn p179-dorun-transient [limit]
  (let [v (transient (vec (repeat limit 1)))]
    (dorun
     (for [i (range 1 limit) j (range i limit i)] 
       (assoc! v j (+ (v j) 1))))
    (persistent! v)))

;(def v (p179 (expt 10 3)))
;(let [w (cons 0 v)] (reduce + (map #(if (= %1 %2) 1 0) v w)))

(defn p179res [limit]
  (let [v (transient (vec (repeat limit 1)))]
    (dorun
     (for [i (range 1 limit) j (range i limit i)] 
       (assoc! v j (+ (v j) 1))))
    (let [v (persistent! v)
	  w (cons 0 v)] (reduce + (map #(if (= %1 %2) 1 0) v w)))))

(defn p179-recur-transient [limit]
  (loop [i 0 d 2 divs (transient (vec (repeat limit 1)))]
    (if (= d limit) 
      (persistent! divs)
      (if (>= i limit) 
	(recur (int 0) (inc d) divs)
	(recur (+ i d) d (assoc! divs i (+ (divs i) 1)))))))

(defn p179-dorun-int-array [limit]
  (let [v (int-array limit 1)]
    (dorun
     (for [i (range 1 limit) j (range i limit i)] 
       (aset v j (+ (aget v j) 1)))) v))

;; A different take on the problem. (inspiring some of the above versions)
;; Now, why does this use less memory as well as being faster?
;; http://github.com/killy971/project-euler/blob/master/clojure/net/projecteuler/179.clj
(defn make-divs-seq [limit]
  (loop [i (int 0), d (int 2), divs (int-array limit 1)]
    (if (= d limit) 
      divs
      (if (>= i limit) 
	(recur (int 0) (inc d) divs)
	(recur (+ i d) d (do (aset divs i (inc (aget divs i))) divs))))))
 
(defn same-cnt [divs]
  (let [divs2 (cons 0 (seq divs))]
    (reduce + (map #(if (= %1 %2) 1 0) divs divs2))))
 
(defn problem-179 []
  (let [divs-seq (make-divs-seq (int 1e7))]
    (same-cnt divs-seq)))

;; p179> (time (problem-179))
;; "Elapsed time: 38507.965323 msecs"
;; 986262

(deftest test-limited-end-result-time
  (let [limit 1000]
    (is (= (time (f1 limit))
	   (time (p179res limit))
	   (time (same-cnt (p179-dorun-transient limit)))	
	   (time (same-cnt (p179-recur-transient limit)))
	   (time (same-cnt (p179-dorun-int-array limit)))
	   (time (same-cnt (take limit (make-divs-seq limit))))))))

(deftest test-resulting-vector
  (let [limit 100]
    (is (= (p179-dorun-transient limit)
	   (vec (take limit (p179-dorun-int-array limit)))))
    (is (= (p179-recur-transient limit) 
	   (vec (take limit (make-divs-seq limit)))))))





