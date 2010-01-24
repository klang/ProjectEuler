(load "tools")
(use 'clojure.set)
(use 'clojure.contrib.combinatorics)
;;A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,

;;a^2 + b^2 = c^2
;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.

;; a<b<c && a^2 + b^2 = c^2 && a+b+c=1000

(defn pyt []
  (loop [r (cartesian-product (range 1 400) (range 2 400))]
    (let [i (first r)
	  a (first i)
	  b (second i)]
      (if (< a b)
	(let [c2 (+ (* a a) (* b b))
	      cr (exact-integer-sqrt c2)]
	  (if (and (zero? (second cr))
		   (= 1000 (+ a b (first cr)))) 
	    (list a b (first cr) (* a b (first cr)))
	    (recur (rest r))
	    ))
	(recur (rest r))
	))))


;; "Elapsed time: 2148.478172 msecs"
;; (200 375 425 31875000)
