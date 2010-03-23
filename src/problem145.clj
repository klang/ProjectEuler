;; Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are reversible. Leading zeroes are not allowed in either n or reverse(n).

;; There are 120 reversible numbers below one-thousand.

;; How many reversible numbers are there below one-billion (10^9)?

(ns p145
  (:use clojure.test)
  (:use [clojure.contrib.math :only (expt)])
  (:use [tools.numbers :only (digit-list integer)]))


(defn reversible-number? [number]
  (if (zero? (mod number 10)) false
      (let [n (digit-list number)
	    r (reverse n)
	    sum  (+ (integer n) (integer r))]
	(every? odd? (digit-list sum)))))

(deftest test-reversible-number
  (is (= 120
	 (count (filter reversible-number? (range 1 1000)))))
  (is (and (reversible-number? 63) (reversible-number? 36)
	   (reversible-number? 409) (reversible-number? 904))))

;; very inefficient. as both parts of a reversible number is calculated
;; ..
;; there must be a system
;;

;;              distinct sums    sums
;;        0        10        0       0   1 digit, none are reversible as all sums are even
;;       10       100        4      20  
;;      100      1000       20     100
;;     1000     10000       20     600
;;    10000    100000        0       0
;;   100000   1000000      100   18000
;;  1000000   2000000        0       0
;;  2000000   3000000      100    2500
;;  3000000   4000000      100    2500
;;  1000000  10000000            22500?
;; 10000000 100000000        ?     

(defn reversible-number-sums [number]
  (if (zero? (mod number 10)) 0
      (let [n (digit-list number)
	    r (reverse n)
	    sum  (+ (integer n) (integer r))]
	(if (every? odd? (digit-list sum)) sum 0)
	)))

;; (def f (sort (filter #(< 0 %) (map  #(reversible-number-sums %) (range 2000000 3000000)))))
;; (def f3 (sort (filter #(< 0 %) (map  #(reversible-number-sums %) (range 3000000 4000000)))))

