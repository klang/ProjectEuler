(ns problem016
  #_(meta {:description "2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

What is the sum of the digits of the number 2^1000?"})
  (:use 
   [clojure.test :only (deftest is)])
  (:require 
   [clojure.string :only (split) :as str]
   [clojure.math.numeric-tower :only (expt) :as math]))

;; based on tools.numbers/digits
(defn solve016 [n] 
  (reduce + 
	(map #(. Integer parseInt % 10) 
	     (filter #(not (= % "")) (str/split (str (math/expt 2 n)) #"")))))

(deftest test-problem016
  (is (= 26 (solve016 15))))

; user> (time (problem016 1000))
; "Elapsed time: 9.828627 msecs"
; 1366

; (run-tests)
(defn problem016 [] (solve016 1000))
