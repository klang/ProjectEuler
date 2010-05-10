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
)

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

;; north east lines can be used to give the coordinates of the points in the 
;; original square, thus converting the square to two triangles, one like the 
;; one in problem 18, one upside down

;; .. 
;; there is no need to convert .. in the example above, we only need to what 
;; values we have on the current north-east-line and the minimized values from
;; the last line to get the minimized values for the next.

(defn value [[x y] square] 
  "returns the value of a point in a square of data" 
  (nth (nth square x) y))

(comment
  (defn squeeze-min [square]
    (let [pre-lines (north-east-lines square)]
      (loop [lines     (drop 2 pre-lines) ;; coordinates refering to the square 
	     minimizedC (first pre-lines) ;; minimum in the top corner is [0 0]
	     minimized (value (first pre-lines) square)
	     line      (second pre-lines) ;; 
	     ]
	(if (empty? lines)
	  minimized
	  (recur (rest lines) 
		 mimimized
		 (first lines)
		 )))))
) 

(comment
  (north-east-lines example)
  ;; [[[0 0]] [[1 0] [0 1]] [[2 0] [1 1] [0 2]] [[3 0] [2 1] [1 2] [0 3]] [[4 0] [3 1] [2 2] [1 3] [0 4]] [[4 1] [3 2] [2 3] [1 4]] [[4 2] [3 3] [2 4]] [[4 3] [3 4]] [[4 4]]]
(map (fn [[x y]] ()) (north-east-lines example))
)

;; It's even possible to just calculate every new value in the square
;; by walking every line in the square .. the easy way

(defn v [[x y] square]
  "calculates the minimum of going from the north-west to the point specified of a square (an nxn matrix)"
  (+ (nth (nth square x) y) 
     (cond 
       (and (zero? x) (zero? y) )    0
       (zero? x) (v [x (dec y)] square)	;(nth (nth square x) (dec y))
       (zero? y) (v [(dec x) y] square);(nth (nth square (dec x)) y)
       :else ; x y
       ;(min ((nth (nth square (dec x)) y) (nth (nth square x) (dec y))))
       (min (v [(dec x) y] square) (v [x (dec y)] square))
       )))

;;(time (v [79 79] s-full))
;; not efficient


;(v i j) + (min (v (dec i) j) (v i (dec j)))
(defn v [[x y] square]
  "returns (v i j) + (min (v (dec i) j) (v i (dec j))) of a point inside the square
But expects another mechanism to sum the results"
  (+ (nth (nth square x) y) 
     (cond 
       (and (zero? x) (zero? y) )    0
       (zero? x) (nth (nth square x) (dec y))
       (zero? y) (nth (nth square (dec x)) y)
       :else ; x y
       (min (nth (nth square (dec x)) y)
	    (nth (nth square x) (dec y))))))

(defn calculate-minimum [square]
  (let [elements (count (first square))]
    (loop [points (for [i (range 0 elements) j (range 0 elements)] [i j])
	   square square] 
      (if (empty? points) 
	    ;; the result will be in the south-west most point
	    (nth (nth square (dec elements)) (dec elements))
	    (let [x (first (first points))
		  y (second (first points))]
	      (recur (rest points)
		     (assoc square x
			    (assoc (nth square x) y (v [x y] square)))))))))

; replace (0,0) with 42
; (assoc example 0 (assoc (nth example 0) 0 42))

;; problem081> (time  (calculate-minimum s-full))
;; "Elapsed time: 81.554255 msecs"
;; 427337


