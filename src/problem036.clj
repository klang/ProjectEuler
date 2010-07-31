(ns problem036
  (meta {:description "
The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

Please note that the palindromic number, in either base, may not include leading zeros."})
  (:use clojure.test))


(defn palindrome? [m]
  (= (apply str (reverse (str m))) (str m)))

(defn base10-and-base2-palindromes [n]
  (filter #(and (palindrome? %) 
		(palindrome? (. Integer toBinaryString %))) 
	  (range n)))

(defn solve036 [n] 
  (reduce + 
	  (base10-and-base2-palindromes n)))

(deftest test-problem036
  (is (and (palindrome? 585) 
	   (palindrome? (. Integer toBinaryString 585)))))

;; user> (time (solve036 1000000))
;; "Elapsed time: 7750.714134 msecs"
;; 872187

;; (run-tests)
(defn problem036 []
  (solve036 1000000))