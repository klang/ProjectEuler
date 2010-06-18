(ns problem085
  (:use clojure.contrib.seq-utils
	clojure.set))
;; 1x1 -> 1
;; 1x2 -> 3
;; 2x2 -> 9
(+ 1 2 
   2 4)
;; 3x2 -> 18
(+ 1 2 3 
   2 4 6)
;; 3x3 -> 36
(+ 1 2 3
   2 4 6
   3 6 9)
;; 4x3 -> 60
(+ 1 2 3  4
   2 4 6  8
   3 6 9 12)
;; 4x4 -> 100
(+ 1 2  3  4
   2 4  6  8
   3 6  9 12
   4 8 12 16)
;; 5x4 -> 150
(+ 1 2  3  4  5
   2 4  6  8 10
   3 6  9 12 15
   4 8 12 16 20)
;; 5x5 -> 225
(+ 1  2  3  4  5
   2  4  6  8 10
   3  6  9 12 15
   4  8 12 16 20
   5 10 15 20 25)
;;(defn flatten [s] (remove seq? (tree-seq seq? seq s)))
(defn row [n m] (range n (inc (* m n)) n))
(defn box [n m] (map #(row % m) (range 1 (inc n))))
(defn sum [a b] 
  "sum of the numbers from a to b"
  (quot (- (+ a b (* b b)) (* a a)) 2))
(defn sum [n] 
  "sum of the numbers from 1 to n"
  (quot (+ n (* n n)) 2))

(defn row-sum [n m] (reduce + (flatten (row n m))))
(defn row-sum-f [n m] 
  "sum of the numbers from 1 to n in steps of m"
  (quot (* n (+ m (* m m))) 2))
(defn box-sum [n m] (reduce + (flatten (box n m))))
(defn box-sum-f [n m] (reduce + (flatten  (map #(row-sum-f % m) (range 1 (inc n))))))

(defn boxes[n]
  (take n (map #(reduce + (flatten (box %1 %2))) 
	       (flatten (map #(repeat 2 %) (iterate inc 1))) 
	       (flatten (lazy-cat [1] (map #(repeat 2 %) (iterate inc 2)))))))

;; 1x1999 -> 1999000 (- 2000000 (sum 1999))
;; 1x2000 -> 2001000 (- 2000000 (sum 2000))
;; 2x1413 -> 1997982 (- 2000000 (reduce + (row 2 1413)))
;; 2x1414 -> 2000810 (- 2000000 (reduce + (row 2 1414)))
;; 3x1154 -> 1999305 (- 2000000 (reduce + (row 3 1154)))
(defn res [n m] (let [s (box-sum-f n m)] (str n"x"m"=" s " " (- 2000000 s))))
(defn foo [n start]
  (let [m (- start (count (take-while #(<= 2000000 %) 
				      (map #(box-sum-f n %) 
					   (iterate dec start)))))
	]
    (println (res n (dec m)) " " (res n m) " " (res n (inc m))))
  )

(comment
(map #(foo % 2000) (range 1 50))
;; inspection
)