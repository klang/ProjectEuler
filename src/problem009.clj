(ns problem009
  (meta {:description "A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

a^2 + b^2 = c^2

For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.

Find the product abc.

a<b<c && a^2 + b^2 = c^2 && a+b+c=1000"})
  (:require
   [clojure.math.combinatorics :only (cartesian-product) :as comb]
   [clojure.math.numeric-tower :only (exact-integer-sqrt) :as math]))

(defn pyt []
  (loop [r (comb/cartesian-product (range 1 400) (range 2 400))]
    (let [i (first r)
	  a (first i)
	  b (second i)]
      (if (< a b)
	(let [c2 (+ (* a a) (* b b))
	      cr (math/exact-integer-sqrt c2)]
	  (if (and (zero? (second cr))
		   (= 1000 (+ a b (first cr)))) 
	    (list a b (first cr) (* a b (first cr)))
	    (recur (rest r))))
	(recur (rest r))))))

;; "Elapsed time: 2148.478172 msecs"
;; (200 375 425 31875000)

(defn problem009 [] (last (pyt)))
