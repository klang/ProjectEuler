(ns problem096
  (meta {:description "brute force sudoku puzzles"})
  (:use tools.numbers
	tools.primes)
  (:use [clojure.contrib.str-utils2 :only (split)]
	clojure.contrib.duck-streams
	clojure.set
	clojure.test))

(defn read-data [filename]
  (partition 10 (split (slurp filename) #"\r\n")))

(def sudoku-data (read-data "sudoku.txt"))

(defn flatten [s] (remove seq? (tree-seq seq? seq s)))

(comment
 (drop 1 (split "003020600" #""))
 (apply str (flatten (drop 1 (first sudoku-data))))
 (map #(flatten (drop 1 %)) sudoku-data) 
 (map #(apply str %) (map #(flatten (drop 1 %)) sudoku-data))
 (map #(apply str %) (map #(drop 1 %) sudoku-data))
)

(defn str2int [string]
  (into [] (map #(. Integer parseInt (str %) 10) (seq string))))

(deftest test-str2int
  (is (= (into [] (map #(. Integer parseInt (str %) 10) (seq "003020600")))
	 (str2int "003020600")
	 [0 0 3 0 2 0 6 0 0])))

;; each sudoko is represented as a string of 81 chars
(def sudoku-string (map #(apply str %) (map #(drop 1 %) sudoku-data)))
;; each sudoko is represented as a vector of 81 elements
(def sudoku-vectors (map #(str2int %) sudoku-string))
;; try to end up with the vectors witout having to 'apply str'
(def sudoku-vector (map #(drop 1 %) sudoku-data))

;; --- at this point, all we have done is read the data and represent it in a managable way.
;;
(def sudoku (first sudoku-vectors))

(defn solve [s])

(deftest test-solve
  (let [sudoku (first sudoku-vectors)]
    (is (= (solve
	    [0 0 3 0 2 0 6 0 0
	     9 0 0 3 0 5 0 0 1
	     0 0 1 8 0 6 4 0 0
	     0 0 8 1 0 2 9 0 0
	     7 0 0 0 0 0 0 0 8
	     0 0 6 7 0 8 2 0 0
	     0 0 2 6 0 9 5 0 0
	     8 0 0 2 0 3 0 0 9
	     0 0 5 0 1 0 3 0 0])

	   [4 8 3 9 2 1 6 5 7
	    9 6 7 3 4 5 8 2 1
	    2 5 1 8 7 6 4 9 3	
	    5 4 8 1 3 2 9 7 6
	    7 2 9 5 6 4 1 3 8
	    1 3 6 7 9 8 2 4 5	
	    3 7 2 6 8 9 5 1 4
	    8 1 4 2 5 3 7 6 9
	    6 9 5 4 1 7 3 8 2]))
    (is (vector? (solve (nth sudoku-vectors 0))))
    (is (vector? (solve (nth sudoku-vectors 1))))))

(comment  
  ;take the 3rd elemnt out of the a sudoko and insert it again
  ;replace the 2nd element with 8, and the 1st element with a 4
  (assoc (first sudoku-vectors) 1 8 0 4)

  ;returns index of first non-zero element  
  (count (take-while #(not (zero? %)) [1 1 1 0 1 1 1 1 1 1]))

  ;returns the first row  
  (take 9 (first sudoku-vectors))
)

(defn row [index sudoku]
  "retuns a vector of the row that contains the index in sudoku"
  (let [q (* 9 (quot index 9))]
    (subvec sudoku q (+ 9 q))))

(comment
  ;returns the first column  
  (map #(nth % 0) (partition 9 (first sudoku-vectors)))
)

(defn column [index sudoku]
  "returns a vector of the column that contains the index in sudoku"
  (map #(nth % (mod index 9 )) (partition 9 sudoku)))

(comment
  ;retuns the first group (not general enough though)
  (flatten (take 3 (map #(take 3 %) (partition 9 (first sudoku-vectors)))))
  (flatten (take 3 (partition 3 9 sudoku)))
  (flatten (take 3 (drop 0 (partition 3 9 sudoku))))
  ;; g00 g01 g02
  ;; g10 g11 g12   gij
  ;; g20 g21 g22
  ;;                     j                      i
  ;;                     |                      |
  ;;                     v                      v
  (flatten (take 3 (drop 0 (partition 3 9 (drop 0 sudoku)))))
)

(defn group [i j sudoku]
  (let [i (* i 3) j (* j 3) ]
    (flatten (take 3 (drop j (partition 3 9 (drop i sudoku)))))))

;; find out which i coordinate an index is located in
(defn i [index] (mod (quot index 3) 3))
(comment
  (defn i [index] (mod (quot index 3) 9))
  (defn i [index] (mod (quot index 9) 3))

  (defn group 
    ([i j sudoku]
       (flatten (take 3 (drop j (partition 3 9 (drop i sudoku))))))
    ([index sudoku]
       (group (i index) (j index))))
)

(deftest test-i
  (is (every? #(= 0 %) (map #(i %) [ 0  1  2  9 10 11 18 19 20])))
  (is (every? #(= 1 %) (map #(i %) [ 3  4  5 12 13 14 21 22 23])))
  (is (every? #(= 2 %) (map #(i %) [ 6  7  8 15 16 17 24 25 26])))

  (is (every? #(= 0 %) (map #(i %) [27 28 29 36 37 38 45 46 47])))
  (is (every? #(= 1 %) (map #(i %) [30 31 32 39 40 41 48 49 50])))
  (is (every? #(= 2 %) (map #(i %) [33 34 35 42 43 44 51 52 53])))

  (is (every? #(= 0 %) (map #(i %) [54 55 56 63 64 65 72 73 74])))
  (is (every? #(= 1 %) (map #(i %) [57 58 59 66 67 68 75 76 77])))
  (is (every? #(= 2 %) (map #(i %) [60 61 62 69 70 71 78 79 80]))))

;; find out which j coordinate an index is located in
(defn j [index] (quot index 27))
(deftest test-j
  (is (every? #(= 0 %) (map #(j %) [ 0  1  2  9 10 11 18 19 20])))
  (is (every? #(= 0 %) (map #(j %) [ 3  4  5 12 13 14 21 22 23])))
  (is (every? #(= 0 %) (map #(j %) [ 6  7  8 15 16 17 24 25 26])))

  (is (every? #(= 1 %) (map #(j %) [27 28 29 36 37 38 45 46 47])))
  (is (every? #(= 1 %) (map #(j %) [30 31 32 39 40 41 48 49 50])))
  (is (every? #(= 1 %) (map #(j %) [33 34 35 42 43 44 51 52 53])))

  (is (every? #(= 2 %) (map #(j %) [54 55 56 63 64 65 72 73 74])))
  (is (every? #(= 2 %) (map #(j %) [57 58 59 66 67 68 75 76 77])))
  (is (every? #(= 2 %) (map #(j %) [60 61 62 69 70 71 78 79 80]))))


(defn group 
  "returns a vector of the group that contains the index or coordinate i,j in sudoku"
  ([i j sudoku]
     (let [i (* i 3) j (* j 3) ]
       (flatten (take 3 (drop j (partition 3 9 (drop i sudoku)))))))
  ([index sudoku] 
     (let [i (mod (quot index 3) 3) j (quot index 27)] 
       (group i j sudoku))))

(deftest test-group
  (let [sudoku (first sudoku-vectors)]
    (is (= '(0 0 3 9 0 0 0 0 1)
	   (flatten (take 3 (drop 0 (partition 3 9 (drop 0 sudoku)))))
	   (group 0 0 sudoku)))
    (is (= '(0 2 0 3 0 5 8 0 6)
	   (flatten (take 3 (drop 0 (partition 3 9 (drop 3 sudoku)))))
	   (group 1 0 sudoku)))
    (is (= '(6 0 0 0 0 1 4 0 0)
	   (flatten (take 3 (drop 0 (partition 3 9 (drop 6 sudoku)))))
	   (group 2 0 sudoku)))
    (is (= '(0 0 8 7 0 0 0 0 6)
	   (flatten (take 3 (drop 3 (partition 3 9 (drop 0 sudoku)))))
	   (group 0 1 sudoku)))
    (is (= '(1 0 2 0 0 0 7 0 8)
	   (flatten (take 3 (drop 3 (partition 3 9 (drop 3 sudoku)))))
	   (group 1 1 sudoku)))
    (is (= '(9 0 0 0 0 8 2 0 0)
	   (flatten (take 3 (drop 3 (partition 3 9 (drop 6 sudoku)))))
	   (group 2 1 sudoku)))
    (is (= '(0 0 2 8 0 0 0 0 5)
	   (flatten (take 3 (drop 6 (partition 3 9 (drop 0 sudoku)))))
	   (group 0 2 sudoku)))
    (is (= '(6 0 9 2 0 3 0 1 0) 
	   (flatten (take 3 (drop 6 (partition 3 9 (drop 3 sudoku)))))
	   (group 1 2 sudoku)))
    (is (= '(5 0 0 0 0 9 3 0 0)
	   (flatten (take 3 (drop 6 (partition 3 9 (drop 6 sudoku)))))
	   (group 2 2 sudoku)))))

(deftest test-group-index
  (let [sudoku (first sudoku-vectors)]
    (is (every? #(= % '(0 0 3 9 0 0 0 0 1)) (map #(group % sudoku) [ 0  1  2  9 10 11 18 19 20])))
    (is (every? #(= % '(0 2 0 3 0 5 8 0 6)) (map #(group % sudoku) [ 3  4  5 12 13 14 21 22 23])))
    (is (every? #(= % '(6 0 0 0 0 1 4 0 0)) (map #(group % sudoku) [ 6  7  8 15 16 17 24 25 26])))
    (is (every? #(= % '(0 0 8 7 0 0 0 0 6)) (map #(group % sudoku) [27 28 29 36 37 38 45 46 47])))
    (is (every? #(= % '(1 0 2 0 0 0 7 0 8)) (map #(group % sudoku) [30 31 32 39 40 41 48 49 50])))
    (is (every? #(= % '(9 0 0 0 0 8 2 0 0)) (map #(group % sudoku) [33 34 35 42 43 44 51 52 53])))
    (is (every? #(= % '(0 0 2 8 0 0 0 0 5)) (map #(group % sudoku) [54 55 56 63 64 65 72 73 74])))
    (is (every? #(= % '(6 0 9 2 0 3 0 1 0)) (map #(group % sudoku) [57 58 59 66 67 68 75 76 77])))
    (is (every? #(= % '(5 0 0 0 0 9 3 0 0)) (map #(group % sudoku) [60 61 62 69 70 71 78 79 80])))))

;; we might as well only use the index part of 'group
(comment
;; not tested and produces a wrong result
  (defn group [index sudoku]
    "returns a vector of the group that contains the index in sudoku"
    (let [i (mod (quot index 3) 3) j (quot index 27)] 
      (flatten (take 3 (drop j (partition 3 9 (drop i sudoku)))))))
  )

(defn candidates [index sudoku]
  "returns a map of possible candidates for an index in sudoku"
  (difference #{1 2 3 4 5 6 7 8 9} 
	      (into #{} (union (row index sudoku) 
			       (column index sudoku)
			       (group index sudoku)))))

(comment
  (def sudoku (first sudoku-vectors))
  (map #(candidates % sudoku) (range 0 81))
  (filter #(not (nil? %)) (map #(if (zero? %1) %2) sudoku (range 0 81)))
  (map #(candidates % sudoku) 
       (filter #(not (nil? %)) 
	       (map #(if (zero? %1) %2) sudoku (range 0 81))))
)

;; given a point in a string, identify which row, column and 3x3 group the point belongs to
;; also identify which candidates numbers fit in that point
;; if only one number fit, insert that number

(comment
  (assoc (first sudoku-vectors) 1 8)
)

(defn queue [sudoku] (filter #(not (nil? %)) (map #(if (zero? %1) %2) sudoku (range 0 81))))

(defn singles [sudoku] 
  "returns a list of pairs of singles in the sudoku. Any cells which have only one candidate can safely be assigned that value"
  (filter #(not (nil? %)) 
	  (map #(let [p (candidates % sudoku)] 
		  (if (= 1 (count p)) (list % (first p)))) (queue sudoku))))

(defn mores [sudoku] (filter #(not (nil? %)) 
			  (map #(let [p (candidates % sudoku)] 
				  (if (< 1 (count p)) (list % (first p)))) (queue sudoku))))

(defn fix-singles [sudoku singles]
  (loop [v sudoku u singles] 
    (if (empty? u) v 
	(recur (assoc v (first (first u)) 
		      (second (first u))) 
	       (rest u)))))

(defn solve [v]
  (loop [sudoku v limit 10]
;    (do (println sudoku) (println (map #(candidates % sudoku) (queue sudoku))))
    (if (or (zero? limit) (empty? (queue sudoku)))
      sudoku
      (let [u (singles sudoku)]
	(if (empty? u)
	  ;; we have to guess, and solve it in a different way
	  sudoku
	  (recur
	   ;; insert the unique solutions in the current suduko, producing a new one
	   (loop [v sudoku u u] 
	     (if (empty? u) v 
		 (recur (assoc v (first (first u)) 
			       (second (first u))) 
			(rest u))))
	   (dec limit)))))))

;; (map #(solve %) sudoku-vectors)
;; (count (filter false? (map #(solve %) sudoku-vectors)))
;; (loop [v v u (singles v)] (if (empty? u) v (recur (assoc v (first (first u)) (second (first u))) (rest u))))

(comment
  (def sudoku (nth sudoku-vectors 1))

  (map #(list % (candidates % sudoku) (count (candidates % sudoku))) (queue sudoku))

  (take-while #(not (false? %)) (drop-while false? (map #(solve (assoc v2 0 %)) (candidates 0 sudoku))))
  (defn missing [sudoku]
    (count (take-while zero? (sort sudoku))))
  (missing sudoku)
  (def sudoku (fix-singles sudoku (singles sudoku)))
  (missing sudoku)

;;hidden singles?
'((1 #{1 4 5 7 9} 5) 
 (2 #{5 7 9} 3) 
 (11 #{5 9} 2) 
 (18 #{1 8} 2) 
 (20 #{7 8} 2) 

 (3 #{4 9} 2) 
 (5 #{1 4 9} 3) 
 (7 #{1 5 7} 3) 
 (8 #{1 5 6 7} 4) 

 (9 #{1 5 9} 3) 
 (12 #{2 3 9} 3) 

 (14 #{1 2 3 9} 4) 
 (15 #{1 5} 2) 


(23 #{1 4} 2) (25 #{1 7} 2) (27 #{6 9} 2) (28 #{7 9} 2) (29 #{3 6 7 9} 4) (31 #{2 3 9} 3) (34 #{2 3 7 9} 4) (36 #{1 5 6 8 9} 5) (37 #{1 5 7 8 9} 5) (38 #{3 5 6 7 8 9} 6) (39 #{2 3 4 8 9} 5) (40 #{2 3 9} 3) (41 #{2 3 4 8 9} 5) (42 #{1 5 6 7 9} 5) (43 #{1 2 3 5 7 9} 6) (44 #{1 2 5 6 7} 5) (46 #{1 5 8 9} 4) (49 #{3 9} 2) (51 #{1 5 9} 3) (52 #{1 3 5 9} 4) (53 #{1 5} 2) (55 #{5 8 9} 3) (57 #{2 6 8 9} 4) (58 #{2 5 9} 3) (60 #{5 8 9} 3) (62 #{2 5} 2) (65 #{5 8 9} 3) (66 #{3 8 9} 3) (68 #{3 8 9} 3) (69 #{1 5 8 9} 4) (71 #{1 5} 2) (72 #{5 6 8 9} 4) (73 #{5 8 9} 3) (75 #{2 6 8 9} 4) (77 #{2 8 9} 3) (78 #{5 7 8 9} 4) (79 #{2 5 7 9} 4))

  )




