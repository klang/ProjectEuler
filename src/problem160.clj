(ns problem160
  (:use tools.numbers)
  (:use clojure.contrib.math)
  (:use clojure.test))

;; http://en.wikipedia.org/wiki/Factorial
;; The asymptotically-best efficiency is obtained by computing n! from its prime factorization.

;; factorial is defined this way, in tools.numbers
;;(defn factorial [n] (reduce * (range n 0 -1)))
;; definately not the correct way to do this, for large numbers.
;; But it gives us the principle.

(defn f [n]
  (integer (reverse 
	    (take 5 
		  (drop-while zero? 
			      (reverse 
			       (digits (factorial n))))))))

(deftest test-f
  (is (= 36288 (f 9)))
  (is (= 36288 (f 10)))
  (is (= 17664 (f 20))))

;; problem160> (factors (expt 10 12))
;; [2 2 2 2 2 2 2 2 2 2 2 2 5 5 5 5 5 5 5 5 5 5 5 5]
;; (* (expt 2 12) (expt 5 12))
;; 1000000000000

;; http://www.luschny.de/math/factorial/FactorialDigits.html
(defn ln [n] (Math/log n))
(def pi (Math/PI))

;b:=2*10^n
;
;ceil(((n*(b+1)+(ln(2*Pi)-b+1/b)/ln(10)))/2)
(defn factorial-digits [n]
  (cond (zero? n) 1
	:else
	(let [b (* 2 (expt 10 n))
	      c (* 1/2 n (+ 1 b))
	      g (- (ln (* 2 pi)) (+ b (/ 1 b)))
	      h (* 1/2 (/ g (ln 10)))]
	  (int (ceil (+ c h))))))

;;problem160> (factorial-digits 12)
;;2147483647
