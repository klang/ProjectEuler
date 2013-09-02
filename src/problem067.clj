;; By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.
;;
;; 3
;; 7 4
;; 2 4 6
;; 8 5 9 3
;;
;; That is, 3 + 7 + 4 + 9 = 23.
(ns problem067
  (:use 
   [clojure.test :only (deftest is)]
   [problem018 :only (read-data squeeze-max-triangle)]))

(def t-mega (read-data "src/triangle.txt"))

;; p018> (time (squeeze-max-triangle t-mega))
;; "Elapsed time: 32.40385 msecs"
;; 7273
(defn problem067 [] (squeeze-max-triangle t-mega))

;; ----- a bit of playing around with graph representations .. might be usefull later
(comment
(deftest test-t
  "small t test to make sure that the ranges make sense" 
  ; The edges on level 0 of the vertex map
  (is (= [[3 7] [3 4]]
	 (vector (vector (nth (t 0) 0) (nth (t 1) 0))    ;       3
		 (vector (nth (t 0) 0) (nth (t 1) 1))))) ;     7   4
  ; another representation of the same edges
  (is (= '([0 0] [0 1])
	  (for [i (range 0 (count (t 0))) j (range i (+ i 2))] [i j])))

  ; The edges on level 1 of the vertex map
  (is (= [[7 2] [7 4]]
	 (vector (vector (nth (t 1) 0) (nth (t 2) 0))    ;     7
		 (vector (nth (t 1) 0) (nth (t 2) 1))))) ;   2   4
  (is (= [[4 4] [4 6]]
	 (vector (vector (nth (t 1) 1) (nth (t 2) 1))    ;         4
		 (vector (nth (t 1) 1) (nth (t 2) 2))))) ;       4   6
  ; again, another representation of the same edges
  (is (= '([0 0] [0 1] [1 1] [1 2])
	 (for [i (range 0 (count (t 1))) j (range i (+ i 2))] [i j])))

  ; The edges on level 2 of the vertex map
  (is (= [[2 8] [2 5]]
	 (vector (vector (nth (t 2) 0) (nth (t 3) 0))    ;   2
		 (vector (nth (t 2) 0) (nth (t 3) 1))))) ; 8   5
  (is (= [[4 5] [4 9]]
	 (vector (vector (nth (t 2) 1) (nth (t 3) 1))    ;       4
		 (vector (nth (t 2) 1) (nth (t 3) 2))))) ;     5   9
  (is (= [[6 9] [6 3]]
	 (vector (vector (nth (t 2) 2) (nth (t 3) 2))    ;           6
		 (vector (nth (t 2) 2) (nth (t 3) 3))))) ;         9   3
  ; and the edges calculated in the same way
  (is (= '([0 0] [0 1] [1 1] [1 2] [2 2] [2 3])
	 (for [i (range 0 (count (t 2))) j (range i (+ i 2))] [i j])))

  ; number of element on each level in the vertex map
  (is (= '(1 2 3 4)
	 (map #(count (t %)) (range 0 (count t)))))
  )

(defn edges [G]
  "return all the edges of G"
  (for [l (range 0 (- (count G) 1)) 
	i (range 0 (count (G l))) 
	j (range i (+ i 2))] 
    [l i j]))

(deftest test-edges
  (is (= '([0 0 0] [0 0 1] [1 0 0] [1 0 1] [1 1 1] [1 1 2] [2 0 0] [2 0 1] [2 1 1] [2 1 2] [2 2 2] [2 2 3])
	 (edges t))))

(defn value [G [l i j]]
  "return the value of the edge"
  (vector (nth (G l) i) (nth (G (+ l 1)) j)))

(deftest test-value
  (is (= [3 7]
	 (value t [0 0 0])))
  (is (= '([3 7] [3 4] [7 2] [7 4] [4 4] [4 6] [2 8] [2 5] [4 5] [4 9] [6 9] [6 3])
	 (map #(value t %) (edges t)))))
)
