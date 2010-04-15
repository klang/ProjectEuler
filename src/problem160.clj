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

;; http://en.wikipedia.org/wiki/Trailing_zeros#Factorial
(defn trailing-zeroes [n]
  (reduce + (map #(floor (/ n (expt 5 %))) 
		 (take-while #(>= n (expt 5 %)) 
			     (iterate inc 1)))))

;; problem160> (trailing-zeroes (expt 10 12))
;; 249999999997

(defn f1 [n]
  (let [a (trailing-zeroes n)
	b1 (expt 10 a)
	b2 (* (expt 10 5) b1)]
    (quot (mod (factorial n) b2) b1)))

;; f1 is much faster than f, but the expensive factorial operation is still done

(deftest test-f1
  (is (= 36288 (f1 9)))
  (is (= 36288 (f1 10)))
  (is (= 17664 (f1 20))))


;; http://www.luschny.de/math/factorial/FastFactorialFunctions.htm

;; problem160> (time (f1 10000))
;; "Elapsed time: 18271.535222 msecs"
;; 79008

(defn special-factorial [n] 
  (reduce * (range n 0 -1)))

(defn s [next so-far] (mod (* next so-far) (expt 10 5)))

(defn f2 [n]
  (let [ex (expt 10 5)]
    (loop [current (range 10 (+ n 1))
	   ;current (range 100000 (+ n 1))
	   ;so-far 62496
	   so-far 36288
	   ]
      (if (empty? current)
	so-far
	(let [candidate (mod (* (first current) so-far) ex)
	      check (mod candidate 10)]
	  (recur (rest current) (if (zero? check) so-far candidate)))))))

(deftest test-f-f1
  (is (every? true? (map #(= (f %) (f1 %)) (range 1 100)))))

(deftest test-f-f1-f2
  (is (every? true? (map #(= (f %) (f1 %) (f2 %)) (range 10 17)))))

(deftest test-f-f1-f2-short
  (is (= (f 15) (f1 15) (f2 15)))
  (is (= (f 16) (f1 16) (f2 16))))

;; problem160> (time (f2 100000))
;; "Elapsed time: 0.29473 msecs"
;; 62496
;; problem160> (time (f2 1000000))
;; "Elapsed time: 2244.540732 msecs"
;; 62496
;; problem160> (time (f2 10000000))
;; "Elapsed time: 28711.281465 msecs"
;; 62496
;; not the correct answer..