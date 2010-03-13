;; For non-negative integers m, n, the Ackermann function A(m, n) is defined as follows:


;; For example A(1, 0) = 2, A(2, 2) = 7 and A(3, 4) = 125.

;;Find 0 n  6 A(n, n) and give your answer mod 148.
(use 'clojure.contrib.test-is)

(defn A [m n]
  (cond (= m 0) (+ n 1)
	(and (< 0 m) (= n 0)) (A (- m 1) 1)
	(and (< 0 m) (< 0 n)) (A (- m 1) (A m (- n 1)))))

;; consuming stack frames as if there is no tomorrow

(deftest test-Ackermann
  (is (= 2 (A 1 0)))
  (is (= 7 (A 2 2)))
  (is (= 125 (A 3 4))))

;;A(m,n)       n=0             n=1             n=2         n=3            n=4         n=5         n=6
;; m=0           1               2               3           4              5           6           7
;; m=1           2               3               4           5              6           7           8
;; m=2           3               5               7           9             11          13          15
;; m=3           5              13              29          61            125         253         509
;; m=4          13           65533        265536-3   2265536-3 A(3,2265536-3) A(3,A(4,4)) A(3,A(4,5))
;; m=5       65533      A(4,65533) A(4,A(4,65533)) A(4,A(5,2))    A(4,A(5,3)) A(4,A(5,4)) A(4,A(5,5))
;; m=6  A(4,65533) A(5,A(4,65533))     A(5,A(6,1)) A(5,A(6,2))    A(5,A(6,3)) A(5,A(6,4)) A(5,A(6,5))

;; ( 1 + 3 + 7 + 61 + A(3,2265536-3) + A(4,A(5,4)) + A(5,A(6,5)) ) mod 14^8

;; http://en.wikipedia.org/wiki/Ackermann_function

;; If we define the function f (n) = A(n, n), which increases both m and n at the same time, we have a function of one variable that dwarfs every primitive recursive function, including very fast-growing functions such as the exponential function, the factorial function, multi- and superfactorial functions, and even functions defined using Knuth's up-arrow notation (except when the indexed up-arrow is used).

;; (reduce + (map #(A % %) (range 0 4)))

;; well .. as we do not have infinite memory on this machine .. 

;; from RosettaCode.org
(defn ackermann [m n] 
  (cond (zero? m) (inc n)
        (zero? n) (ackermann (dec m) 1)
        :else (ackermann (dec m) (ackermann m (dec n)))))