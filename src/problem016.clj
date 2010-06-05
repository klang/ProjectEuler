(ns problem016
  (meta {:description "2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?"})
  (:use clojure.test)
  (:use [clojure.contrib.math :only (expt)])
  (:use [clojure.contrib.str-utils2 :only (split)]))


(defn digits [n]
  (map #(. Integer parseInt % 10) 
	     (filter #(not (= % "")) (split (str n) #""))))

(defn problem016 [n] 
  (reduce + 
	(map #(. Integer parseInt % 10) 
	     (filter #(not (= % "")) (split (str (expt 2 n)) #"")))))

(deftest test-problem016
  (is (= 26 (problem016 15))))

; user> (time (problem016 1000))
; "Elapsed time: 9.828627 msecs"
; 1366

; (run-tests)
