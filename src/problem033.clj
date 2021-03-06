(ns problem033
  (meta {:description "
The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
 
If the product of these four fractions is given in its lowest common terms, find the value of the denominator."})
  (:use 
   [clojure.test :only (deftest is)]
   [tools.numbers :only (digits integer)])
  (:require
   [clojure.set :only (intersection difference) :as set]))

;; (count (for [d (range 10 100) n (range 10 d)] [n d]))
;; 4005

;; al the fractions in the search space
(def f (for [d (range 10 100) 
	     n (range 10 d) 
	     :when (not (and (zero? (mod n 10)) 
			     (zero? (mod d 10))))] 
	 [n d]))

;; the fractions that can be reduced in the traditional way
(def d (distinct (for [d (range 10 100) n 
		       (range 10 d) 
		       :when (not (and (zero? (mod n 10)) 
			     (zero? (mod d 10))))] 
		   (/ n d))))

(defn unorthodox-cancelling? [[ numerator denominator]]
  (let [n (set (digits numerator)) 
	d (set (digits denominator))
	i (set/intersection n d)
	nd (set/difference n i)       
	dd (set/difference d i)
	ndx (if (empty? nd) i nd) 
	ddx (if (empty? dd) i dd)]
    (cond (= n d) false
	  (zero? (first (seq ddx))) false
	  :else
	  (and (not (= n ndx)) (not (= d ddx)) 
	       (= (/ numerator denominator) 
		  (/ (first (seq ndx)) (first (seq ddx))))))))

(deftest test-unorthodox-cancelling?
  (is (not (unorthodox-cancelling? [10 11])))
  (is (not (unorthodox-cancelling? [11 20])))
  (is (not (unorthodox-cancelling? [44 55])))
  (is (not (unorthodox-cancelling? [49 94])))
  (is (unorthodox-cancelling? [49 98]))
  (is (unorthodox-cancelling? [30 50]))
  (is (not (unorthodox-cancelling? [49 99]))))


;; user> (filter #(unorthodox-cancelling? %) f)
;; ([16 64] [26 65] [19 95] [49 98])
;; user> (map (fn [[n d]] (/ n d)) (filter #(unorthodox-cancelling? %) f))
;; (1/4 2/5 1/5 1/2)
;; user> (reduce * (map (fn [[n d]] (/ n d)) (filter #(unorthodox-cancelling? %) f)))
;; 1/100

;; beautiful code in Haskell
;; xy/yz = x/z
;; (10x + y)/(10y+z) = x/z
;; 9xz + yz = 10xy
;; 
;;[(10*x+y,10*y+z) | x <- [1..9], y <- [1..9], z <- [1..9], x /= y , (9*x*z) + (y*z) == (10*x*y)]

;; beautiful translation to Clojure
(defn solve033 []
  (for [x (range 1 10) y (range 1 10) z (range 1 10) 
	:when (and (not (= x y)) (= (+ (* 9 x z) (* y z)) (* 10 x y)))]
    [(+ (* 10 x) y) (+ (* 10 y) z)]))

(defn problem033 []
  (reduce * (map #(/ (first %) (second %)) (solve033))))
