;; By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
;; 
;; 3
;; 7 4
;; 2 4 6
;; 8 5 9 3
;; 
;; That is, 3 + 7 + 4 + 9 = 23.
;; 
;; Find the maximum total from top to bottom of the triangle below:
;; 
;; [snipped]
;; 
;; NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)
(ns problem018
  (:use clojure.test)
  (:use [clojure.contrib.str-utils2 :only (split)]))

(defn str2int [v]
  (map #(. Integer parseInt % 10) v))

(defn read-data [filename]
  (into [] (map #(into [] (str2int %)) 
		(map #(split % #" ") (split (slurp filename) #"\n")))))

(def t-hard [[3] [7 4] [2 4 6] [8 5 9 3]])
(def t-read (read-data "triangle-018-test.txt"))
(def t-full (read-data "triangle-018.txt"))

(deftest test-read-data (is (= t-read t-hard)))

(max (+ 3 7 2) (+ 3 7 4) (+ 3 4 4) (+ 3 4 6))   ; lookahead = 2 elements
;;      0 0 0    [0 0 1]    0 1 1     0 1 2     ; item number

(max (+ 3 7 2 8) (+ 3 7 2 5) (+ 3 7 4 5) (+ 3 7 4 9 ) (+ 3 4 4 5) (+ 3 4 4 9) (+ 3 4 6 9) (+ 3 4 6 3))
;;      0 0 0 0     0 0 0 1     0 0 1 1    [0 0 1 2]     0 1 1 1     0 1 1 2     0 1 2 2     0 1 2 3

;;      0 0 0    [0 0 1]    0 1 1     0 1 2     ; item number
(max (+ 3 7 2) (+ 3 7 4) (+ 3 4 4) (+ 3 4 6))   ; lookahead = 2 elements

(def t002 {[0 0 0] [3 7 2] [0 0 1] [3 7 4] [0 1 1] [3 4 4] [0 1 2] [3 4 6]})
(def c (into {} (map #(vector % (reduce + (t002 %))) (keys t002))))

(defn lookahead-max [triangle] nil)

(defn path-elements  [triangle level element lookahead])

(defn paths [triangle level element lookahead]
  "paths going out from an 'element on 'level of length 'lookahead"
  ;; paths going out from 'element and 'element on 'level+1 of length 'lookahead-1
  ;(cond (zero? lookahead) ())
  )

(defn reduce-1-level [triangle]
  (cons (into [] (for [a (first triangle)
		       b (second triangle)] (+ a b))) (drop 2 triangle)))
(comment
  (def triangle [[3] [7 4] [2 4 6] [8 5 9 3]])
  (def triangle [[10 7] [2 4 6] [8 5 9 3]])
  (def triangle [[12 14 13] [8 5 9 3]])
  (def level1 (first triangle))
  (def level2 (second triangle)))

(defn level2-max [level1 level2]
  ;; (list (max (+ 5 12) (+ 5 14)) (max (+ 9 14) (+ 9 13)))
  "constructs a level2 with max taken between each element and the two elements 'above' from level 1. first and last element will be missing. see next-level2"
  (for [i (range 1 (- (count level2) 1))] 
    (max (+ (level1 i) (level2 i)) 
	 (+ (level1 (- i 1)) (level2 i)))))

;; TODO: find a better way to create a vector from several parts
(defn next-level2 [level1 level2]
  "constructs a new level2 based on level1 and level2. Edge elements are glued onto the result from level2-max."
  (vec (concat (into  [(+ (first level1) (first level2))]
		      (level2-max level1 level2))
	       [(+ (last level1) (last level2))])))

(deftest test-next-level2
  (is (= (next-level2 [3] [7 4]) [10 7]))
  (is (= (next-level2 [10 7] [2 4 6]) [12 14 13]))
  (is (= (next-level2 [12 14 13] [8 5 9 3]) [20 19 23 16])))

(defn squeeze-max [triangle]
  "squeezes the first two levels of a triangle into a new triangle with running maxes calculated for each element"
  (let [level1 (first triangle)      ;  [2 4 6]
	level2 (second triangle)     ; [8 5 9 3]
	next-level (next-level2 level1 level2)]
    (into [next-level2] (drop 2 triangle))))

(defn squeeze-max [triangle]
  "squeezes the first two levels of a triangle into a new triangle with running maxes calculated for each element"
  (into [(next-level2 (first triangle) (second triangle))] (drop 2 triangle)))

;; (reduce max (first (squeeze-max (squeeze-max (squeeze-max triangle)))))
;; 23

(defn squeeze-max-triangle [triangle]
  (loop [triangle (squeeze-max triangle)]
    (if (= 1 (count triangle))
      ;(do (println triangle)) 
      (reduce max (first triangle))
      ;(do (println triangle))
      (recur (squeeze-max triangle)))))

;; p018> (time (squeeze-max-triangle t-full))
;; "Elapsed time: 0.885029 msecs"
;; 1074

;; directly usable for problem067