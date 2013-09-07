(ns problem030
  (meta {:description "n! means n  (n  1)  ...  3  2  1

Find the sum of the digits in the number 100!"})
  (:use 
   [clojure.test :only (deftest is)]
   [tools.numbers :only (digits)])
  (:require 
   [clojure.math.numeric-tower :only (expt) :as math]
   [clojure.string :only (split) :as str]))

(defn sum-of-powers [pow number]
  (reduce + (map #(math/expt % pow) (digits number))))

(defn sum-of-powers? [pow number]
  (= number (sum-of-powers pow number)))

;;; The maximum value for one digit is 9^5 = 59049. We can find out the maximum possible sum for a given number of digits by multiplying 59049 with the number of digits. 
;; Let's say we're gonna check the number 123456789. That's 9 digits, so the maximum sum would be 9*59049 = 531441, which doesn't even come close to 123456789. So we know we can forget about any number 9-digit number because we'll never be able to reach a big enough sum. And it'll only get worse with larger numbers :)
;; Actually.. the limit could also be 6*9^5=354294. 
(defn limit [n pow] (* (count (digits n)) (math/expt (count (digits n)) pow)))

(filter #(sum-of-powers? 4 %) (range 2 10000))
(filter #(sum-of-powers? 5 %) (range 2 1000000))
;; (4150 4151 54748 92727 93084 194979)
; (filter #(sum-of-powers? 5 %) (range 2 1000000))
; (4150 4151 54748 92727 93084 194979)
; (filter #(sum-of-powers? 5 %) (range 2 100000000))
;; stopped .. takes too long
;; (filter #(sum-of-powers? 5 %) (range 1000000 2000000))
;; ()

(defn problem030 [n] 
  (reduce + 
	(filter #(sum-of-powers? n %) (range 2 1000000))))

(deftest test-problem030
  (is (sum-of-powers? 4 1634))
  (is (sum-of-powers? 4 8208))
  (is (sum-of-powers? 4 9474))
  (is (= 19316 (problem030 4)))
  )

; user> (time (problem030 5))
;; "Elapsed time: 6952.531 msecs" ;; (re-run 2013 on 8 core MacBook Pro)
;; 443839
;; simply expect there to be no other candidates below 1000000 .. cheating for sure
;; (reduce + '(4150 4151 54748 92727 93084 194979))
;; 443839
; (run-tests)
