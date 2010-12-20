(ns problem068
  (meta {:description ""})
  (:use [tools.numbers :only (digits integer)]
	[clojure.contrib.combinatorics]
	[clojure.test]))

;; Total	Solution Set
;; 9	        [[4 2 3] [5 3 1] [6 1 2]]
;;                0 1 2   3 2 4   5 4 1
;;               n0 n1 n2 n3 n2 n4 n5 n4 n1
;;               n0+n1+n2=n3+n2+n4=n5+n4+n1

(defn gon3
  "produces 3 symetric result sets (works 3 times as much as neccesary)"
  []
  (for [[n0 n1 n2 n3 n4 n5] (permutations [1 2 3 4 5 6])
	:when (and (= (+ n0 n1 n2)
		      (+ n3 n2 n4)
		      (+ n5 n4 n1)))]
    (let [m (min n0 n3 n5)]
      (cond (= m n0) (integer [n0 n1 n2 n3 n2 n4 n5 n4 n1])
	    (= m n3) (integer [n3 n2 n4 n5 n4 n1 n0 n1 n2])
	    :else (integer [n5 n4 n1 n0 n1 n2 n3 n2 n4])))))

(deftest test-gon3
  (= 432621513 (apply max (distinct (gon3)))))

(defn i [v]
  (BigInteger. (apply str v)))

(defn gon5
  "to produce 16 digit numbers, 10 has to be one of the external nodes"
  []
  (for [[n0 n1 n2 n3 n4 n5 n6 n7 n8 n9] (permutations (range 1 11))
	:when (and (or (= n0 10) (= n3 10) (= n5 10) (= n7 10) (= n9 10))
		   (= (+ n0 n1 n2)
		      (+ n3 n2 n4)
		      (+ n5 n4 n6)
		      (+ n7 n6 n8)
		      (+ n9 n8 n1)))]
    (let [m (min n0 n3 n5 n7 n9)]
      (cond
       (= m n0) (i [n0 n1 n2 n3 n2 n4 n5 n4 n6 n7 n6 n8 n9 n8 n1])
       (= m n3) (i [n3 n2 n4 n5 n4 n6 n7 n6 n8 n9 n8 n1 n0 n1 n2])
       (= m n5) (i [n5 n4 n6 n7 n6 n8 n9 n8 n1 n0 n1 n2 n3 n2 n4])
       (= m n7) (i [n7 n6 n8 n9 n8 n1 n0 n1 n2 n3 n2 n4 n5 n4 n6])
       :else    (i [n9 n8 n1 n0 n1 n2 n3 n2 n4 n5 n4 n6 n7 n6 n8])))))

;; some of the numbers are way too big to be integers .. function i does not work

;; problem068> (time (apply max (distinct (gon5))))
;; "Elapsed time: 81512.760641 msecs"
;; 6531031914842725
(defn problem068 [] (apply max (distinct (gon5))))

;; 10 seconds faster, if n0 is locked to 10 (the solutions are symetric)

;; for the inner ring
;; (combinations [1 2 3 4 5 6 7 8 9 10] 5)
;; for the outer ring
;; (difference #{1 2 3 4 5 6 7 8 9 10} (first (combinations [1 2 3 4 5 6 7 8 9 10] 5)))