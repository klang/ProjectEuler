(ns problem197
  (meta
   {:description "Given is the function f(x) = floor(2^30.403243784-x²)  10^-9
the sequence un is defined by u(0) = -1 and u(n+1) = f(u(n)).

Find u(n) + u(n+1) for n = 10^12.
Give your answer with 9 digits after the decimal point." 
    })
  (:use 
   [clojure.math.numeric-tower :only (floor expt)]
   [clojure.test :only (deftest is)]))

(defn f [x] (* (floor (expt 2 (- 30.403243784 (expt x 2)))) (expt 10 -9)))

(defn u [] (map first (iterate (fn [[a b]] [b (f b)]) [-1 (f -1)])))

;; problem197> (time (nth (u) (expt 10 4)))
;; "Elapsed time: 2140.189129 msecs"
;; 1.029461839
;; problem197> (time (nth (u) (expt 10 5)))
;; "Elapsed time: 21453.263646 msecs"
;; 1.029461839


;; problem197> (time (nth (u) (+ 1 (expt 10 4))))
;; "Elapsed time: 2629.685276 msecs"
;; 0.681175878
;; problem197> (time (nth (u) (+ 1 (expt 10 5))))
;; "Elapsed time: 21678.678676 msecs"
;; 0.681175878

;; it probably coverges .. 

;; problem197> (+ 1.029461839 0.681175878)
;; 1.710637717

;; Excellent work, klang! By solving your 100th problem you have earned a place among the 
;; top 2.48% of members and have advanced to level 3.

(defn problem197 [] (+ (nth (u) (expt 10 5)) (nth (u) (+ 1 (expt 10 4)))))
