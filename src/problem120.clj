(ns problem120
  (:use clojure.contrib.math)
  (:use clojure.test))

;; Let r be the remainder when (a-1)^n + (a+1)^n is divided by a².

;; For example, if a = 7 and n = 3, then r = 42: 63 + 83 = 728  42 mod 49. 
;; And as n varies, so too will r, but for a = 7 it turns out that r-max = 42.

;; For 3 <= a <= 1000, find sum r_max.

(defn euler [a n]
  (rem (+ (expt (- a 1) n) (expt (+ a 1) n)) (* a a)))

(defn maximum [n]
  (last (sort (distinct (map #(euler n %) (range 1 (* 2 n)))))))

;; problem120> (maximum 7)
;; 42

;; a little faster
(defn maximum [n]
  (reduce #(max %1 %2) (map #(euler n %) (range 1 (* 2 n)))))

;; faster still .. as we notice that euler = 2 for even values
(defn maximum [n]
  (reduce #(max %1 %2) (map #(euler n %) (range 1 (* 2 n) 2))))

;; problem120> (time (reduce + (map maximum (range 3 1001))))
;; "Elapsed time: 608337.113359 msecs"
;; 333082500

;; (a+1)^n == an+1 (mod a^2), and (a-1)^n == an-1 or 1-an (mod a^2) depending whether n is odd or even; the sum is therefore either 2an or 2. 

;; When a is odd, this is always maximised at a^2-a (as in the example with a=7), achieved for example when n=(a-1)/2; when a is even, it is maximised at a^2-2a for a>2, achieved for example when n=(a-2)/2. 

;; The separate sums for odd and even terms are easily enough calculated, but it was quicker to knock out a line of code: 

;; perl -wle '$s+=$_*($_-2+($_&1)) for 3..1000; print $s' 

(
 (reduce + (map #(- (expt % 2) (* (- 2 (mod % 2)) %)) (range 3 1001)))
)
