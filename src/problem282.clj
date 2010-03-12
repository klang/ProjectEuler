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

;; http://en.wikipedia.org/wiki/Ackermann_function

;; If we define the function f (n) = A(n, n), which increases both m and n at the same time, we have a function of one variable that dwarfs every primitive recursive function, including very fast-growing functions such as the exponential function, the factorial function, multi- and superfactorial functions, and even functions defined using Knuth's up-arrow notation (except when the indexed up-arrow is used).

;; (reduce + (map #(A % %) (range 0 4)))

;; well .. as we do not have infinite memory on this machine .. 