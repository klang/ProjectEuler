(ns problem096
  (meta {:description "brute force sudoku puzzles"})
  (:use tools.numbers
	tools.primes)
  (:use [clojure.contrib.str-utils2 :only (split)]
	clojure.contrib.combinatorics
	clojure.contrib.duck-streams
	clojure.set
	clojure.test))

(defn read-data [filename]
  (partition 10 (split (slurp filename) #"\r\n")))

(def sudoku-data (read-data "sudoku.txt"))

(comment
  (defn flatten [s] (remove seq? (tree-seq seq? seq s)))
  (defn flatten
    "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns nil."
    [x]
    (filter (complement sequential?)
	    (rest (tree-seq sequential? seq x))))
  )

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
  (if (zero? (sudoku index))
    (difference #{1 2 3 4 5 6 7 8 9} 
		(into #{} (union (row index sudoku) 
				 (column index sudoku)
				 (group index sudoku))))
        (hash-set (sudoku index))))

(comment
  (def sudoku (first sudoku-vectors))
  (map #(candidates % sudoku) (range 0 81))
  (filter #(not (nil? %)) (map #(if (zero? %1) %2) sudoku (range 0 81)))
  (map #(candidates % sudoku) 
       (filter #(not (nil? %)) 
	       (map #(if (zero? %1) %2) sudoku (range 0 81))))
)

(defn queue [sudoku] 
  "return the indexes for unknown numbers in the sudoku"
  (filter #(not (nil? %)) (map #(if (zero? %1) %2) sudoku (range 0 81))))

;;--------------------find and elliminate singles------------------------------
;; given a point in a string, identify which row, column and 3x3 group the point belongs to
;; also identify which candidates numbers fit in that point
;; if only one number fit, insert that number

(defn singles [sudoku] 
  "returns a list of pairs of singles in the sudoku. Any cells which have only one candidate can safely be assigned that value"
  (filter #(not (nil? %)) 
	  (map #(let [p (candidates % sudoku)] 
		  (if (= 1 (count p)) (list % (first p)))) (queue sudoku))))

(defn fix-singles [sudoku]
  "insert the unique solutions in the current suduko, producing a new one"
  (loop [v sudoku u (singles sudoku)] 
    (if (empty? u) v 
	(recur (assoc v (first (first u)) 
		      (second (first u))) 
	       (rest u)))))

(defn elliminate-singles [sudoku]
  "keep inserting unique solutions in the current suduko, until there are no more singles"
  (loop [s sudoku p nil]
    (if (or (empty? (queue s)) (= p s)) 
      s
      (recur (fix-singles s) s))))
;;-----------------------------------------------------------------------------
(def g [4 7 0 
	3 8 0 
	2 0 0])

(def c {2 #{1 5 9} 5 #{1 5 6 9} 7 #{1 9} 8 #{1 5 9}})

(comment
  ;; select one .. diff it with the union of the rest
  (difference (c 5) (union (c 2) (c 7) (c 8))) ;; <<-- 6 is a hidden singel for group resolve it and check the group again
  (difference (c 2) (union (c 5) (c 7) (c 8)))
  (difference (c 7) (union (c 2) (c 5) (c 8)))
  (difference (c 8) (union (c 2) (c 7) (c 5)))
)
;; groups start at indexes 
;; 0 3 6 27 30 33 54 57 60
(def group-start 
   (flatten (map #(list % (+ % 3) (+ % 6)) (filter #(zero? (mod % 27)) (range 0 81)))))

;; expensive way to calculate a group, but I can't be bothered anymore .. 
(def group-indices
     (map #(group % (range 0 81)) group-start))

(defn groups [sudoku]
  "returns the nine groups in a sudoku"
  (map #(group % sudoku) group-start))

(defn all-candidates [sudoku]
  (flatten-once (map #(list % (candidates % sudoku)) (queue sudoku))))

(comment
  (into {} (map #(hash-map % (candidates % (nth sudoku-vectors 8))) [ 0  1  2  9 10 11 18 19 20] ))
  (into {} (map #(hash-map % (candidates % (nth sudoku-vectors 8))) (group 0 (range 0 81)))))


(defn group-queue [group-index sudoku]
  "return the elements from a group, that are still not determined"
  (intersection (set (group group-index (range 0 81))) (set (queue sudoku))))

(comment
  (flatten-once (map #(list % (candidates % (nth sudoku-vectors 8))) (group-index 0 sudoku)))
  (into {} (map #(hash-map % (candidates % (nth sudoku-vectors 8))) (group-queue 0 sudoku))))

(defn group-candidates [group-index sudoku]
  (into {} (map #(hash-map % (candidates % (nth sudoku-vectors 8))) (group-queue group-index sudoku))))

(comment
  (into {} (map (fn [d] (hash-map d (remove (fn [u] (= d u)) (keys c)))) (keys c)))
)

(defn one-and-the-rest [gc]
  (into {} (map (fn [d] (hash-map d (remove (fn [u] (= d u)) (keys gc)))) (keys gc))))

(comment
  (def gc8 (group-candidates 0 (nth sudoku-vectors 8)))
  (difference (c 5) (union (c 2) (c 7) (c 8)))
  (remove empty? (map (fn [[one the-rest]] (difference 
					    (gc8 one) 
					    (reduce union (map #(gc8 %) the-rest)))) (one-and-the-rest gc8)))
  (remove empty? (map (fn [[one the-rest]] (difference 
					    (c one) 
					    (reduce union (map #(c %) the-rest)))) (one-and-the-rest c)))

  ;; last one here gives the correct result, but the index is missing

  (into {} (map (fn [[one the-rest]] (hash-map one (difference 
						    (c one) 
						    (reduce union (map #(c %) the-rest))))) (one-and-the-rest c)))

  ;; index available .. now we just have to remove the ones with empty sets

  (into {} 
	(remove #(empty? (val %)) 
		(into {} 
		      (map (fn [[one the-rest]] 
			     (hash-map one (difference 
					    (c one) 
					    (reduce union (map #(c %) the-rest))))) 
			   (one-and-the-rest c)))))
)

(defn hidden-single [group-index sudoku]
  (let [c (group-candidates group-index sudoku)]
    (into {} 
	  (remove #(empty? (val %)) 
		  (into {} 
			(map (fn [[one the-rest]] 
			       (hash-map one (difference 
					      (c one) 
					      (reduce union (map #(c %) the-rest))))) 
			     (one-and-the-rest c)))))))

(defn hidden-singles [sudoku]
  (into {} (remove empty? (map #(hidden-single % sudoku) group-start))))
;;
;; ONLY RESOLVE ONE HIDDEN SINGLE AT THE TIME, NOT ALL OF THEM
;;
(defn fix-hidden-singles [sudoku]
  "insert the unique solutions in the current suduko, producing a new one"
  (loop [v sudoku u (hidden-singles sudoku)] 
    (if (empty? u) v 
	(recur (assoc v (first (first u)) 
		      (first (second (first u)))) 
	       (rest u)))))

(defn elliminate-hidden-singles [sudoku]
  "keep inserting unique solutions in the current suduko, until there are no more singles"
  (loop [s sudoku p nil]
    (if (or (empty? (queue s)) (= p s)) 
      s
      (recur (fix-hidden-singles s) s))))



(comment
  (all-hidden-singles (nth sudoku-vectors 8)))

;;(group 0 (nth sudoku-vectors 8))
;   (0 0 0 
;    0 5 0 
;    0 3 0)

;;(flatten-once (map #(list % (candidates % (nth sudoku-vectors 8))) [ 0  1  2  9 10 11 18 19 20] ))
;; candidates should return the locked value for index 10 and 19 in this case
;{0 #{1 6 7 8} 1 #{1 4 6 8} 2 #{4 6} 9 #{6 7 8} 10 #{6 8} 11 #{6 9} 18 #{2 7 8} 19 #{2 4 8} 20 #{2 4 9}}


;;-----------------------------------------------------------------------------
;; if nothing else works, take a wild guess
(defn guesses [sudoku]
  (let [paths (map #(list % (candidates % sudoku)) (queue sudoku))]
    (map (fn [[index cs]] (map #(assoc sudoku index %) cs)) paths)))

(defn flatten-once [s] (remove seq? (tree-seq seq? seq s)))


(defn solve [sudoku]
  (loop [s sudoku]
    (let [partial (elliminate-singles s)]
      (if (empty? (queue partial))
	;; no elements in queue => nothing else to do
	partial
	;; try to guess, and solve
	(let [suggestion (first (remove false? (map solve (flatten-once (guesses partial)))))]
	  (if suggestion suggestion false))))))


(defn total-count [sudokus]
  (reduce + (map #(integer %) (map #(take 3 %) (map solve sudokus)))))

(comment
  (defn total-count-stats [sudokus]
    (reduce + (map #(integer %) (map #(take 3 %) (map #(time (solve %)) sudokus)))))
  (time
   (total-count-stats (map #(nth sudoku-vectors %) 
			   [0 1 2 3 4 5 6 7 9 10 11 12 13 14 15 16 18 19 20 
			    21 22 23 25 26 30 31 32 33 34 35 37 38 39 42 44 46 ])))

  (def t1 [82.702993 578.789654 257.70714 449.061673 61.546669 776.079569 205.393495 69.821464 459.322472 755.007885 159.267227 394.162767 1443.685281 140.630214 47.616402 47.616402 54.095997 130.004543 118.481842 189.754349 306.004328 242.261038 237.079345 311.052205 442.426466 442.426466 463.144188 517.4715 341.684297 101.087185 190.925992 75.471339 117.777573 159.035349 99.078541 3056.712249 509.248384 413.33282])
  ;;"Elapsed time: 14014.579823 msecs"
  ;;14293

  (time (total-count-stats (map #(nth sudoku-vectors %) [24 27 28 36 49])))
  (def t2 [1758.692202 2210.227051 6041.850865 16178.975875 15745.116404])
  ;;"Elapsed time: 41938.368431 msecs"
  ;;1223
  ;; (+ (reduce + t1) (reduce + t2))
  ;; 56381.8297 msecs used .. almost out of time

  ;; the hard ones that are left:
  ; 8 17 40 41 43 44 45 47 48 49

  (defn method-solve [sudoku]
    (loop [s sudoku]
      (let [partial (elliminate-singles s)]
	(if (empty? (queue partial))
	  ;; no elements in queue => nothing else to do
	  true
	  ;; could not solve by one of the known methods
	  false)))))
(count (remove false? (map method-solve sudoku-vectors)))


;; with just brute force, there are some sudokus that take quite a while to solve
;; problem096> (time (sudoku.core/solve (elliminate-singles (nth sudoku-vectors 8))))
;; "Elapsed time: 123463.773906 msecs"

;; with singles ellimination and brute force .. we do much worse
;; problem096> (time (solve (nth sudoku-vectors 8)))


(comment
  (def sudoku (nth sudoku-vectors 1))

  (map #(list % (candidates % sudoku) (count (candidates % sudoku))) (queue sudoku))

  (take-while #(not (false? %)) (drop-while false? (map #(solve (assoc v2 0 %)) (candidates 0 sudoku))))
  (defn missing [sudoku]
    (count (take-while zero? (sort sudoku))))
  (missing sudoku)
  (def sudoku (fix-singles sudoku (singles sudoku)))
  (missing sudoku)

  (defn hidden-singles []
    (map #(list % (candidates % sudoku) (count (candidates % sudoku))) (queue sudoku)))
;  (defn locked-candidates-1)
;  (defn locked-candidates-2)
;;hidden singles?

  )




