(ns problem052
  (:use [tools.numbers :only (digit-set)]))

;; It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.

;; Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
(use 'clojure.set)

(defn p052number [number]
  (list number (* 2 number) (* 3 number) (* 4 number) (* 5 number) (* 6 number)))

(defn same-digits [d-set1 d-set2]
  (and (empty? (difference d-set1 d-set2)) 
       (empty? (difference d-set2 d-set1))))

(defn same-digits [number1 number2]
  (let [ds1 (digit-set number1)
	ds2 (digit-set number2)]
    (and (empty? (difference ds1 ds2)) 
	 (empty? (difference ds2 ds1)))))

(defn same-digits [d-set number]
  (let [ds2 (digit-set number)]
    (and  (empty? (difference d-set ds2))
	  (empty? (difference ds2 d-set )))))

(defn p052o [number]
  (let [d (digit-set number)]
    (and (same-digits d (* number 2)) 
	 (same-digits d (* number 3)) 
	 (same-digits d (* number 4)) 
	 (same-digits d (* number 5)) 
	 (same-digits d (* number 6)))))

;;user> (time (first (take 1 (filter #(p052o %) (iterate inc 1)))))
;;"Elapsed time: 2848.972985 msecs"
;;142857
;; user> (p052number 142857)
;; (142857 285714 428571 571428 714285 857142)

(defn problem052 [] (first (take 1 (filter #(p052o %) (iterate inc 1)))))
