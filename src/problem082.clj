(ns problem082
  (meta {:description "The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the left column and finishing in any cell in the right column, and only moving up, down, and right, is indicated in red and bold; the sum is equal to 994.

201 96 342 234 103 18 = 994

Find the minimal path sum, in matrix.txt, a text file containing a 80 by 80 matrix, from the left column to the right column"})
  (:use tools.numbers)
  (:use tools.primes)
  (:use [clojure.contrib.str-utils2 :only (split)])
  (:use clojure.contrib.duck-streams)
  (:use clojure.test))

(defn str2int [v]
  (map #(. Integer parseInt % 10) v))

(defn read-data [filename]
  (into [] (map #(into [] (str2int %)) 
		(map #(split % #",") (split (slurp filename) #"\r\n")))))

(def s-full (read-data "matrix.txt"))

(def example [[131 673 234 103  18]
	      [201  96 342 965 150]
	      [630 803 746 422 111]
	      [537 699 497 121 956]
	      [805 732 524  37 331]])

(def path [201 96 342 234 103 18])

;(map #(reduce + %) example)
;(1159 1754 2712 2810 2429)
;(reduce min  (map #(reduce + %) example))
;1159
;(reduce min  (map #(reduce + %) s-full))
;349326
