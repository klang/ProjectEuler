(ns problem073
  (meta {:description "Consider the fraction, n/d, where n and d are positive integers. If nd and HCF(n,d)=1, it is called a reduced proper fraction.
 
If we list the set of reduced proper fractions for d <  8 in ascending order of size, we get:
 
 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
 
It can be seen that there are 3 fractions between 1/3 and 1/2.
 
How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d <= 12,000?"})
  (:use clojure.contrib.math)
  (:use tools.numbers)
  (:use tools.primes)
  (:use problem072)
  (:use tools.farey)
  (:use clojure.test))
 
(defn fractions [min max limit] 
  (sort (for [d (range 1 (+ limit 1)) 
	      n (range 1 d) 
	      :when (and (= 1 (gcd n d)) 
			 (< min (/ n d) max))] 
	  (/ n d))))

(deftest test-fractions
   (is (= 3) (count (fractions 1/3 1/2 8))))

;; user> (time (count (fractions 1/3 1/2 1000)))
;; "Elapsed time: 5334.214105 msecs"
;; 50695
;; .. method not efficient for target limit (takes way more than a minute)


;; (sort (for [d (range 1 8) n (range 1 d) :when (= 1 (gcd n d))] (/ n d)))
;; (find-max 3 7 8)

;; user> (time (find-max 3 7 1000000))
;; "Elapsed time: 1867.093735 msecs"
;; 428570/999997

;;-------------

;; from problem 72, we have the number of reduced fractions in the entire range
;; (time (- (F 12000) 2))
;; "Elapsed time: 1010.601969 msecs"
;; 43772257
;; we also know, that 1/2 is always the middle element in farley series.

(comment
  (defn farey [n]
    (map #(/ (first %) (second %)) 
	 (iterate 
	  (fn [[a b c d]] 
	    (let [k (int (/ (+ n b) d))] 
	      [c d (- (* k c) a) (- (* k d) b)])) [0 1 1 n]))))

(deftest test-fractions
  (is
   (= 3
      (count (fractions 1/3 1/2 8))
      (count (drop-while #(<= % 1/3)  (take-while #(< % 1/2) (farey 8))))))
  (is
   (= 505
      (count (fractions 1/3 1/2 100))
      (count (drop-while #(<= % 1/3)  (take-while #(< % 1/2) (farey 100))))))
  (is
   (= 50695
      (count (fractions 1/3 1/2 1000))
      (count (drop-while #(<= % 1/3)  (take-while #(< % 1/2) (farey 1000))))))
  (comment
    (is
     (= 202766
	(count (fractions 1/3 1/2 2000))
	(count (drop-while #(<= % 1/3)  (take-while #(< % 1/2) (farey 2000))))))))

;; to avoid realizing the entire sequence before being able to count it
;; we wrap and count
(defn count-fractions []
  (loop [fractions (drop-while #(<= % 1/3) 
			       (take-while #(< % 1/2) (farey 12000)))
	 number 0]
    (if (empty? fractions)
      number
      (recur (rest fractions) (inc number)))))
;; (time (count-fractions))
;; "Elapsed time: 364597.112721 msecs"
;; 7295372

(comment
(drop-while #(fraction-compare % [1 3])
	    (take-while #(fraction-compare % [1 2])
			(farey-seq 8)))
)

(defn count-fractions-faster []
  (loop [fractions (drop-while #(fraction-compare % [1 3])
	    (take-while #(fraction-compare % [1 2])
			(farey-seq 12000)))
	 number 0]
    (if (empty? fractions)
      number
      (recur (rest fractions) (inc number)))))

;; .. well, not really faster. Got bored waiting.