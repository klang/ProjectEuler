(ns problem015
  (:use 
   [clojure.test :only (deftest is)]
   [tools.numbers :only (factorial)]
   [clojure.math.numeric-tower :only (expt)]))

;; next possible steps from a point in a max X max square
(defn path-transition [[i j] max]
  (cond (and (= max i) (= max j)) []
	(= max j) [[(inc i) j]]
	(= max i) [[i (inc j)]]
	:else (list [i (inc j)] [(inc i) j])))

(deftest test-path-transition
  (is (= (list [0 1] [1 0]) (path-transition [0 0] 3)))
  (is (= (list [2 3] [3 2]) (path-transition [2 2] 3)))
  (is (= (list [3 3]) (path-transition [3 2] 3)))
  (is (= (list [3 3]) (path-transition [2 3] 3)))
  (is (= (list) (path-transition [3 3] 3))))

(defn next-part [coll max]
  (for [next-elements (path-transition (last coll) max)
	:when (< 0 (count next-elements))] 
    (conj coll next-elements)))

(defn pt [max]
  (loop [paths [[[0 0]]]]
    (if (every? #(= [max max] %) (map #(last %) paths))
      (count paths)
      (recur (vec (mapcat #(next-part % max) paths)))
      )))

;; (map  #(pt %) (range 0 10))
;; (1 2 6 20 70 252 924 3432 12870 48620)

;; generating the routes before only using the count is a seriously bad idea.

;; After having used too much time on this, let's look if there is a sequence..

;; http://www.research.att.com/~njas/sequences/A000984

;; C(2n,n) = (2n)!/(n!)^2.
(defn C [n]
  (/ (factorial (* 2 n)) (expt (factorial n) 2)))

;; user> (time (C 20))
;; "Elapsed time: 0.763784 msecs"
;; 137846528820

(defn problem015 [] (C 20))

;;It can indeed be solved in one equation, and curiously enough, the solution to it is in one of the other problems. The solution lies in the N choose R formula. For any number of rows R, you take (2R!)/(R! ^ 2). This comes from the equation n choose r = n!/r!(n-r)!, and you replace n with 2r, for the number of rows problem. :) 

;;P.S. I liked the hint "rth element in the nth row"! ;)

