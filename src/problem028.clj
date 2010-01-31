;Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:

;21 22 23 24 25
;20  7  8  9 10
;19  6  1  2 11
;18  5  4  3 12
;17 16 15 14 13

;It can be verified that the sum of the numbers on the diagonals is 101.

;What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?

;73 74 75 76 77 78 79 80 81
;72 43 44 45 46 47 48 49 50
;71 42 21 22 23 24 25 26 51
;70 41 20  7  8  9 10 27 52
;69 40 19  6  1  2 11 28 53
;68 39 18  5  4  3 12 29 54
;67 38 17 16 15 14 13 30 55
;66 37 36 35 34 33 32 31 56
;65 64 63 62 61 60 59 58 57

;;        1 3  5  7                          9
(def se '(1 3 13 31)) ; 2 10 18 26 --> 26+31=57
(def sw '(1 5 17 37)) ; 4 12 20 28 --> 28+37=65
(def nw '(1 7 21 43)) ; 6 14 22 30 --> 30+43=73
(def ne '(1 9 25 49)) ; 8 16 24 32 --> 32+49=81

;(range 8 100 8) --> (8 16 24 32 40 48 56 64 72 80 88 96)
;                  1  9 25 49  
;; good old fibo .. 
(defn ne []
  (map first (iterate (fn [[a b]] [(+ a b) (+ 8 b)]) [1 8])))
(defn nw []
  (map first (iterate (fn [[a b]] [(+ a b) (+ 8 b)]) [1 6])))
(defn sw []
  (map first (iterate (fn [[a b]] [(+ a b) (+ 8 b)]) [1 4])))
(defn se []
  (map first (iterate (fn [[a b]] [(+ a b) (+ 8 b)]) [1 2])))

(defn nxn [n]
  (count (filter odd? (range 1 (+ 1 n)))))

(defn diagonal-sum [n]
  (- 
   (reduce + (take (nxn n) (map + (ne) (nw) (sw) (se)))) 
   3)) ; the first item is counted 4 times

;; user> (time (diagonal-sum 1001))
;; "Elapsed time: 59.577705 msecs"
;; 669171001

;; faster to simply calculate all four sequences in one go..
;; looking at the time to calculate 1001x1001, we will wait
;; with this improvement

