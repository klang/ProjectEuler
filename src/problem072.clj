(ns problem072
  (meta {:description "Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

If we list the set of reduced proper fractions for d  8 in ascending order of size, we get:

1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

It can be seen that there are 21 elements in this set.

How many elements would be contained in the set of reduced proper fractions for d <= 1,000,000?"
	 :hint "http://en.wikipedia.org/wiki/Farey_sequence"})
  (:use clojure.contrib.math)
  (:use tools.numbers)
  (:use tools.primes)
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
