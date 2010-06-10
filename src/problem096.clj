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

(defn solve [s])

(deftest test-solve
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
	  6 9 5 4 1 7 3 8 2])))


;; --- at this point, all we have done is read the data and represent it in a managable way.
;;

  ;take the 3rd elemnt out of the a sudoko and insert it again
  ;replace the 2nd element with 8, and the 1st element with a 4

(comment  
  (assoc (first sudoku-vectors) 1 8 0 4)
  ;returns index of first non-zero element
  
  (count (take-while #(not (zero? %)) [1 1 1 0 1 1 1 1 1 1]))
  ;returns the first row
  
  (take 9 (first sudoku-vectors)))

(defn row [index v]
  "retuns a vector of the row that contains the index in v"
  (let [q (* 9 (quot index 9))]
    (subvec v q (+ 9 q))))

(comment
  ;returns the first column  
  (map #(nth % 0) (partition 9 (first sudoku-vectors))))

(defn column [index v]
  "returns a vector of the column that contains the index in v"
  (map #(nth % (mod index 9 )) (partition 9 v)))

(comment
  ;retuns the first group (not general enough though)
  
  (flatten (take 3 (map #(take 3 %) (partition 9 (first sudoku-vectors)))))
  (flatten (take 3 (partition 3 9 v)))
  (flatten (take 3 (drop 0 (partition 3 9 v))))
  ;; g00 g01 g02
  ;; g10 g11 g12   gij
  ;; g20 g21 g22
  ;;                     j                      i
  ;;                     |                      |
  ;;                     v                      v
  
  (flatten (take 3 (drop 0 (partition 3 9 (drop 0 v))))))

(defn group [i j v]
  (let [i (* i 3) j (* j 3) ]
    (flatten (take 3 (drop j (partition 3 9 (drop i v)))))))

(defn i [index] (mod (quot index 3) 3))

(comment
  (defn i [index] (mod (quot index 3) 9))
  (defn i [index] (mod (quot index 9) 3))
  (defn group 
    ([i j v]
       (flatten (take 3 (drop j (partition 3 9 (drop i v))))))
    ([index v]
       (group (i index) (j index)))))

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
  ([i j v]
     (let [i (* i 3) j (* j 3) ]
       (flatten (take 3 (drop j (partition 3 9 (drop i v)))))))
  ([index v] 
     (let [i (mod (quot index 3) 3) j (quot index 27)] 
       (group i j v))))



(deftest test-group
  (is (= '(0 0 3 9 0 0 0 0 1)
	 (flatten (take 3 (drop 0 (partition 3 9 (drop 0 v)))))
	 (group 0 0 v)
	 ))
  (is (= '(0 2 0 3 0 5 8 0 6)
	 (flatten (take 3 (drop 0 (partition 3 9 (drop 3 v)))))
	 (group 1 0 v)))
  (is (= (flatten (take 3 (drop 0 (partition 3 9 (drop 6 v)))))
	 (group 2 0 v)
	 (every? '(6 0 0 0 0 1 4 0 0)
		 (map #(group % v) [ 6  7  8 15 16 17 24 25 26])))
      )

  (is (= '(0 0 8 7 0 0 0 0 6)
	 (flatten (take 3 (drop 3 (partition 3 9 (drop 0 v)))))
	 (group 0 1 v)))
  (is (= '(1 0 2 0 0 0 7 0 8)
	 (flatten (take 3 (drop 3 (partition 3 9 (drop 3 v)))))
	 (group 1 1 v)))
  (is (= '(9 0 0 0 0 8 2 0 0)
	 (flatten (take 3 (drop 3 (partition 3 9 (drop 6 v)))))
	 (group 2 1 v)))

  (is (= '(0 0 2 8 0 0 0 0 5)
	 (flatten (take 3 (drop 6 (partition 3 9 (drop 0 v)))))
	 (group 0 2 v)))
  (is (= '(6 0 9 2 0 3 0 1 0) 
	 (flatten (take 3 (drop 6 (partition 3 9 (drop 3 v)))))
	 (group 1 2 v)))
  (is (= '(5 0 0 0 0 9 3 0 0)
	 (flatten (take 3 (drop 6 (partition 3 9 (drop 6 v)))))
	 (group 2 2 v))))

(defn group [index v]
  (let [i (mod (quot index 3) 3) j (quot index 27)] 
    ;; calculate group coordinate based on index
    (flatten (take 3 (drop j (partition 3 9 (drop i v)))))))


(defn possible [index v]
  (difference #{1 2 3 4 5 6 7 8 9} 
	      (into #{} (union (row index v) 
			       (column index v)
			       (group index v)))))

(comment
  (def v (first sudoku-vectors))
  (map #(possible % v) (range 0 81))
  (filter #(not (nil? %)) (map #(if (zero? %1) %2) v (range 0 81)))
  (map #(possible % v) 
       (filter #(not (nil? %)) 
	       (map #(if (zero? %1) %2) v (range 0 81)))))

;; given a point in a string, identify which row, column and 3x3 group the point belongs to
;; also identify which possible numbers fit in that point
;; if only one number fit, insert that number


