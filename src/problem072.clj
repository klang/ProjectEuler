(ns problem072
  (meta {:description "Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d  8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 21 elements in this set.

How many elements would be contained in the set of reduced proper fractions for d <= 1,000,000?"
	 :hint "http://en.wikipedia.org/wiki/Farey_sequence"})
  (:use clojure.contrib.math)
  (:use :reload-all tools.numbers)
  (:use :reload-all tools.primes)
  (:use [clojure.contrib.lazy-seqs :only (primes)])
  (:use clojure.test))


(def f1 [0/1 1/1])
(def f2 [0/1 1/2 1/1])
(def f3 [0/1 1/3 1/2 2/3 1/1])
(def f4 [0/1 1/4 1/3 1/2 2/3 3/4 1/1])
(def f5 [0/1 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1/1])
(def f6 [0/1 1/6 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 5/6 1/1])
(def f7 [0/1 1/7 1/6 1/5 1/4 2/7 1/3 2/5 3/7 1/2 4/7 3/5 2/3 5/7 3/4 4/5 5/6 6/7 1/1])
(def f8 [0/1 1/8 1/7 1/6 1/5 1/4 2/7 1/3 3/8 2/5 3/7 1/2 4/7 3/5 5/8 2/3 5/7 3/4 4/5 5/6 6/7 7/8 1/1]) ; 0<=n<=d

(defn F-asymptotic [n] (/ (* 3 n n) (* Math/PI Math/PI)))

;; (totient 1) = 1
(defn F [n] (+ 1 (+ 1 (reduce + (map #(totient %) (range 2 (inc n)))))))

(deftest test-F
  (is (= (count f1) (F 1)))
  (is (= (count f2) (F 2)))
  (is (= (count f3) (F 3)))
  (is (= (count f4) (F 4)))
  (is (= (count f5) (F 5)))
  (is (= (count f6) (F 6)))
  (is (= (count f7) (F 7)))
  (is (= (count f8) (F 8))))

;; problem072> (time (- (F 1000000) 2))
;; "Elapsed time: 215707.091089 msecs"
;; 303963552391

;; maybe a more efficient totient function would be in order?

;;

(defn a [n]
  (meta {:description ""
	 :url "http://www.research.att.com/~njas/sequences/A005728"
	 :formular "a(n) = 1+Sum_{i=1..n} phi(i).
a(n) = n(n+3)/2 - Sum(k = 2 to n, a([n/k])). - David W. Wilson, May 25, 2002"})
  (- (/ (* n (+ n 3)) 2) 
     (reduce + (map (fn [k] (a (int (/ n k)))) (range 2 (inc n))))))

;; n 1000              10000              1000000
;; F  84.25376 msecs     963.177193 msecs  215707.091089
;; a 486.153576 msecs  25622.780795 msecs  -
(def a (memoize a))
;; a 61.883309 msecs     404.298746 msecs   64763.338642 msecs

;; implementation of an algorithm from the forum

(meta {:description "I calculated the sum of totients in less than 1 second. Basically my algorithm was a modification of sieve. 

In this initialize all elements below the limit with the number itself. Tot=i 

start from p=2 increment in steps of p and for every step 
Tot*=(p-1)/(p) 
Now search for entry where Tot=i this corresponds to a prime put this as p,repeat the above procedure in steps of p,Tot*=(p-1)/(p) until p exeeds 1,000,000."})

;; (time (def tots (int-array 40000000 (iterate inc 1))))
;; "Elapsed time: 34747.914056 msecs"

(def totseq (lazy-cat (list 1) (map #(totient %) (iterate inc 2))))
(comment
  (defn tots-seq2 [limit]
    (loop [i (int) tots (int-array limit (iterate inc 1))]
    
      )))

(defn make-tots-seq-prime [limit]
  "proof of concept. Primes are implicitly found by the method. Here, generated twice."
  (loop [i (int 0), ps (rest primes), p (int 2), tots (int-array limit (iterate inc 0))]
    (if (or (<= limit p)) tots 
      (if (<= limit i) 
	(recur (int 0) (rest ps) (int (first ps)) tots)
	(recur (+ i p) ps p (do (aset tots i (int (* (aget tots i) (/ (- p 1) p)))) tots))))))

;; (time (- (reduce + (make-tots-seq-prime 1000000) 1)))
;; "Elapsed time: 9590.604915 msecs"
;; 303963152391
;; better performance than the original algorithm for this problem

(defn search [n coll]
  "search coll from the n'th element, for the first element that equals it's index."
  (first (drop-while nil? (map #(if (= %1 %2) % nil) 
				     (drop n coll) (iterate inc n)))))
(defn make-tots-seq [limit]
  (loop [i (int 0), p (int 2), tots (int-array limit (iterate inc 0))]
    (if (or (<= limit p) (zero? p))
      tots
      (if (<= limit i) 
	(recur (int 0) (int (let [pp (search p tots)] (if (nil? pp) 0 pp))) tots)
	(recur (+ i p) p (do (aset tots i (int (* (aget tots i) (/ (- p 1) p)))) tots))))))

;; not impressing
;; problem072> (time (count (make-tots-seq 100000)))
;; "Elapsed time: 100495.628014 msecs"
;; 100000
;; problem072> (time (count (make-tots-seq-prime 100000)))
;; "Elapsed time: 1603.569119 msecs"
;; 100000

(deftest test-tots
  (is (= [1 1 2 2 4 2 6 4 6 4 10 4 12 6 8 8 16 6 18 8 12 
	  10 22 8 20 12 18 12 28 8 30 16 20 16 24 12 36 18 
	  24 16 40 12 42 20 24 22 46 16 42 20 32 24 52 18 
	  40 24 36 28 58 16 60 30 36 32 48 20 66 32 44 24]
	 (take 70 (tots-seq 100))
	 (take 70 (drop 1 (make-tots-seq-prime 100)))))
  (is (= (take 70 (tots-seq 100))
	 (take 70 (drop 1 (make-tots-seq 100))))))

(defn search [n coll]
  "search coll from the n'th element, for the first element that equals it's index."
  (first (drop-while nil? (map #(if (= %1 %2) % nil) 
				     (drop n coll) (iterate inc n)))))
;(def t [0 1 1 2 4 4 2 7 4 6 4 11])
;(search 5 t)
(defn search-for-index-from-i [i coll] ;; add limit to avoid count
  (loop [i (int i)] (if (< i (count coll)) 
		      (if (= i (aget coll i))
			i 
			(recur (inc i)))
		      0)))

(defn mark-off [n limit tots] 
  "demonstrates part of the algorithm"
  (loop [i (int 0) p (int n) tots tots]
    (if (<= limit i)
      tots
      (recur (+ i p) p (do (aset tots i (int (* (aget tots i) (/ (- p 1) p)))) tots)))))

(deftest test-mark-off-and-search
  (let [t2 (int-array 15 (iterate inc 0))]
    (do (mark-off 2 15 t2) (mark-off 3 15 t2)) ;; mark off 2 and 3
    (is (= 5 (search-for-index-from-i 3 t2)))    ;; next prime is 5
    (do (mark-off 5 15 t2))                     ;; mark off 5
    (is (= 7 (search-for-index-from-i 5 t2)))    ;; next prime is 7
    ))

(defn make-tots-seq2 [limit]
  (let [tots (int-array limit (iterate inc 0))]
    (loop [i (int 0), p (int 2) ] ; note p does not need to be int?
      (if (or (<= limit p) (zero? p))
	tots
	(if (<= limit i) 
	  (recur (int 0) (int (search-for-index-from-i p tots)))
	  (do (aset tots i (int (* (aget tots i) (/ (- p 1) p))))
	      (recur (+ i p) p)))))))

;;problem072> (time (count (make-tots-seq2 1000000)))
;;"Elapsed time: 9701.404796 msecs"
;;1000000

(defn search-for-index-from-i [i coll]
  (loop [i (int i)] (if (< i (count coll)) 
		      (if (= i (aget coll i))
			i 
			(recur (inc i)))
		      (+ (count coll) 1)))) 

;; return limit + 1 to avoid zero? check

(defn make-tots-seq3 [limit]
  (let [tots (int-array limit (iterate inc 0))]
    (loop [i (int 0), p (int 2) ]
      (if (<= limit p)
	tots
	(if (<= limit i) 
	  (recur (int 0) (int (search-for-index-from-i p tots)))
	  (do (aset tots i (int (* (aget tots i) (/ (- p 1) p))))
	      (recur (+ i p) p)))))))

;; no significan change.
;; tried parsing limit to search function .. no change
;; tried ignoring typecast on p .. no change

;; problem072> (time (count (make-tots-seq3 1000000)))
;; "Elapsed time: 10648.358859 msecs"
;; 1000000

