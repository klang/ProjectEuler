(ns problem125
  (meta {:description "The palindromic number 595 is interesting because it can be written as the sum of consecutive squares: 6² + 7² + 8² + 9² + 10² + 11² + 12².

There are exactly eleven palindromes below one-thousand that can be written as consecutive square sums, and the sum of these palindromes is 4164. Note that 1 = 0² + 1² has not been included as this problem is concerned with the squares of positive integers.

Find the sum of all the numbers less than 10⁸ that are both palindromic and can be written as the sum of consecutive squares."})
  (:use clojure.contrib.lazy-seqs
	clojure.contrib.seq-utils
	clojure.contrib.math
	tools.numbers
	clojure.test))

;; 1 4  9 16 25 36  49  64  81 100 121 144 169  196  225
;; 1 5 14 30 55 91 140 204 285 385 506 650 819 1015 1240 

(defn squared-sums []
  (drop 1 (map first (iterate (fn [[a b]] [(+ a (* b b)) (inc b)]) [0 1]))))

(defn squared-sum-palindromes [n]
  (filter #(< 0 %) 
	  (filter palindrome? 
		  (take-while #(< % 1000) (map #(- % n) (squared-sums))))))

(map #(squared-sum-palindromes %) (take-while #(< % 1000) (squared-sums)))
;; 1 2  3  4  5 6    7   8   9  10  11  12  13   14   15
;; 1 5 14 30 55 91 140 204 285 385 506 650 819 1015 1240 
;; 1           5   14       30 55
;; 1           2   3        4  5     6  7  8     9  10        11
;;((4 505 818) (9) (77 636) () (595) () () (181) () (121 434) (313) () ())
;; 
