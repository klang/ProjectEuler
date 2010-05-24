(ns problem225
  (meta {:description "The sequence 1, 1, 1, 3, 5, 9, 17, 31, 57, 105, 193, 355, 653, 1201 ...
is defined by T1 = T2 = T3 = 1 and Tn = Tn-1 + Tn-2 + Tn-3.

It can be shown that 27 does not divide any terms of this sequence.
In fact, 27 is the first odd number with this property.

Find the 124th odd number that does not divide any terms of the above sequence."})
  (:use clojure.test))

(defn T [n]
  (cond (= n 1) 1
	(= n 2) 1
	(= n 3) 1
	:else
	(+ (T (- n 1)) (T (- n 3)) (T (- n 3)))))

(def T (memoize T))

(defn Ts []
  (lazy-cat [1] 
	    (map first (iterate (fn [[a b c]] [c (+ a b) (+ a b c)]) [1 1 1]))))
;(1 1 1 3 5 7 13 23 37 63 109 183 309 527 893)
problem225> (+ 1 1 1)
3
problem225> (+ 1 1 3)
5
problem225> (+ 1 3 5)
9
problem225> (+ 3 5 9)
17
