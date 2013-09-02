(ns problem004
  (meta {:description "A palindromic number reads the same both ways. 

The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 * 99.

Find the largest palindrome made from the product of two 3-digit numbers."})
  (:use clojure.test)
  (:require [clojure.math.numeric-tower :only (expt) :as math]))

(defn palindrome? [m]
  (= (apply str (reverse (str m))) (str m)))


(comment 
(reduce max (for [x (range 10 100) 
		  y (range 10 (+ x 1))] 
	      (if (palindrome? (* x y)) (* x y) 0)))
)

(comment 
(reduce max (for [x (range 100 1000) 
		  y (range 100 (+ x 1))] 
	      (if (palindrome? (* x y)) (* x y) 0)))
)

(defn n-digit-range [n]
  (range (math/expt 10 (- n 1)) (math/expt 10 n)))

;; n specifies the number of digits in the number
;; x*y = y*x, so we only need to let y run up to and including x
;; we save about half of the calculations.
(defn- solve004 [n]
  (reduce max (for [x (range (math/expt 10 (- n 1)) (math/expt 10 n)) 
		    y (range (math/expt 10 (- n 1)) (+ x 1))] 
		(if (palindrome? (* x y)) (* x y) 0))))

(deftest test-problem004
  (is (= 9009 (solve004 2))))

; user> (time (solve004 3))
; "Elapsed time: 3624.171717 msecs"
; 906609

(defn problem004 [] (solve004 3))
