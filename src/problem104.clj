(ns problem104
  (:use [tools.numbers :only (digits fibos)]
        [clojure.math.numeric-tower :only (expt)]))
;; The Fibonacci sequence is defined by the recurrence relation:

;;  F(n) = F(n-1) + F(n-2), where F(1) = 1 and F(2) = 1.
;; It turns out that F(541), which contains 113 digits, is the first Fibonacci number for which the last nine digits are 1-9 pandigital (contain all the digits 1 to 9, but not necessarily in order). And F(2749), which contains 575 digits, is the first Fibonacci number for which the first nine digits are 1-9 pandigital.

;; Given that F(k) is the first Fibonacci number for which the first nine digits AND the last nine digits are 1-9 pandigital, find k.

(defn least-9-digits [number]
  (digits (rem number 1000000000)))

(defn least-pandigital-1-9? [number]
  (let [s (set (least-9-digits number))]
    (and (not (contains? s 0)) (= 9 (count s)))))

;; (str number) is a lazy sequence over the number
;; taking 9 should be reasonably fast, even if we
;; calculate the same split via 'digits'
(defn most-pandigital-1-9? [number]
  (let [m (. Integer parseInt (apply str (take 9 (str number))) 10)
	s (set (take 9 (digits m)))]
    (and (not (contains? s 0)) (= 9 (count s)))))
;; .. but it is not ..

;; user> (least-pandigital-1-9? (nth (fibos) 541))
;; true
;; user> (most-pandigital-1-9? (nth (fibos) 2749))
;; true

(defn p104 []
  (loop [fib (fibos)
	 n 0]
    (if (and (least-pandigital-1-9? (first fib)) 
	     (most-pandigital-1-9? (first fib)))
      n
      (recur (rest fib) (inc n)))))

(defn p104printing []
  (loop [fib (fibos)
	 n 0]
    (if (least-pandigital-1-9? (first fib))
      (do (println n)
	(if (most-pandigital-1-9? (first fib))
	  n
	  (recur (rest fib) (inc n))))
      (recur (rest fib) (inc n))
      )))

;; hopelessly inefficient
;;"Elapsed time: 467170.32899 msecs"
;; 329468

;;; -- forum optimations..
(def phi (/ (+ 1 (Math/sqrt 5)) 2))
(defn fibFirst10 [i]
  ;; the first 10 digits in the result will be the same
  ;; as the first 10 digits in the i-th fibonacci number
  (let [tmp (+ (* i (Math/log10 phi)) 
	       (Math/log10 (/ 1 (Math/sqrt 5))))]
    (expt 10 (- tmp (+ (Math/floor tmp) 8)))))

(defn first10digits [k-th-fibo]
  (map #(. Integer parseInt (str %) 10) 
	     (filter #(not (= % \.)) 
		     (take 10 (str (fibFirst10 k-th-fibo))))))

(defn first-fib-pandigital? [k-th-fibo]
  (let [s (set 
	   (map #(. Integer parseInt (str %) 10) 
		(filter #(not (= % \.)) 
			(take 10 (str (fibFirst10 k-th-fibo))))))]
    (and (not (contains? s 0)) (= 9 (count s)))))

(defn p104 []
  (loop [fib (fibos)
	 n 0]
    (if (and (least-pandigital-1-9? (first fib)) 
	     (first-fib-pandigital? n))
      n
      (recur (rest fib) (inc n)))))

;; user> (time (p104))
;; "Elapsed time: 197231.24822 msecs"
;; 329468

;; optimized? .. not much
(defn problem104 [] (p104))
