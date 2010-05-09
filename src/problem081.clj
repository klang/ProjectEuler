(ns problem081
  (meta {:description "In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by only moving to the right and down, is indicated in bold red and is equal to 2427.

131 201 96 342 746 422 121 37 331 = 2427

Find the minimal path sum, in matrix.txt, a text file containing a 80 by 80 matrix, from the top left to the bottom right by only moving right and down."})

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

(def path [131 201 96 342 746 422 121 37 331])

(comment
  ; general idea is the same as problem 18 at least until the diagonal
  (list (conj (drop 2 (first example)) (+ (nth (first example) 0) (nth (first example) 1))) 
	(conj (drop 1 (second example)) (+ (nth (first example) 0) (nth (second example) 0))))
  (min (+ (nth (first example) 0) (nth (first example) 1)) 
       (+ (nth (first example) 0) (nth (second example) 0) ))
  ;; squeeze the starting element (i j) into it's neighbours at (i j+1) and (i+1 j)
  (def exampl1 [[  0 804 234 103  18]
		[332  96 342 965 150]
		[630 803 746 422 111]
		[537 699 497 121 956]
		[805 732 524  37 331]])
  ;; squeeze coordinate (i+1 j+1)' = (min (+ (i+1 j+1) (i+1 j)) (+ (i+1 j+1) (i j+1))) 
  ;            j   0   1   2   3   4   
  (def exampl1 [[  0 804 234 103  18]
		[332  96 342 965 150]
		[630 803 746 422 111]
		[537 699 497 121 956]
		[805 732 524  37 331]])
  ;; squeeze (i+1 j+1)' = (min (+ (i+1 j+1) (i+1 j)) (+ (i+1 j+1) (i j+1))) 
  ;;         (i j) into it's neighbours at (i j+1) and (i+1 j)
  (def exampl2 [[  0  0 1068 103  18]
		[  0 428 342 965 150]
		[962 803 746 422 111]
		[537 699 497 121 956]
		[805 732 524  37 331]])

  ;; diagonals of a 5x5 square
  ;;  [[0 0]                        ]
  ;;  [[1 0] [0 1]                  ] 
  ;;  [[2 0] [1 1] [0 2]            ]
  ;;  [[3 0] [2 1] [1 2] [0 3]      ]
  ;;  [[4 0] [3 1] [2 2] [1 3] [0 4]]
  ;;  [      [4 1] [3 2] [2 3] [1 4]]
  ;;  [            [4 2] [3 3] [2 4]]
  ;;  [                  [4 3] [3 4]]
  ;;  [                        [4 4]]
)
(defn north-east-line [[x y]]
  "returns the points of the north-east-line going north east, starting in (x y)"
  (loop [i (range x (+ y -1) -1) 
	 j (range y (+ x 1)) 
	 d []] 
    (if (empty? i) d
	(recur (rest i) (rest j) (conj d [(first i) (first j)])))))

(defn north-east-lines [square]
  "returns the north-east-lines of a square"
  ;(count (first square))
  (let [elements (count (first square))]
    (loop [i (range 0 elements)
	   j (range 1 elements) ;; start from 1 to avoid doubling the north-east-line
	   d []]
      (if (empty? i)
	(if (empty? j) d
	    (recur i (rest j) (conj d (north-east-line [(- elements 1) (first j)]))))
	(recur (rest i) j (conj d (north-east-line [(first i) 0])))))))


(defn squeeze-min [square]
  "squeezes a square from the top left")


