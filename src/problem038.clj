(ns problem038
  (meta {:description "Take the number 192 and multiply it by each of 1, 2, and 3:

      192 x 1 = 192
      192 x 2 = 384
      192 x 3 = 576
      By concatenating each product we get the 1 to 9 pandigital, 192384576. We will call 192384576 the concatenated product of 192 and (1,2,3)
 
      The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645, which is the concatenated product of 9 and (1,2,3,4,5).

      What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n) where n > 1?"})
  (:use clojure.contrib.combinatorics
	tools.numbers
	clojure.test))

(deftest test-examples
  (is (= 9 (count (reduce concat (map digits (map #(* 192 %) '(1 2 3)))))))
  (is (= 9 (count (reduce concat (map digits (map #(* 9 %) '(1 2 3 4 5))))))))

(defn pandigital? [number]
  (= 9 (count (distinct (digits number)))))

(def perms  (map integer (permutations '(9 8 7 6 5 4 3 2 1))))
;; number of permutations is of course (factorial 9)

;; find a permutation, p that can be split in n pieces, where
(defn task [p m n]
  (and (= (digits p) 
	  (reduce concat (map digits (map #(* m %) (range 1 (+ n 1)))))) 
       (pandigital? p)))

(deftest test-task
  (is (task 192384576 192 3))
  (is (task 918273645 9 5)))

;; 918273645 is obviously not the correct answer so the answer is larger than that.
;; so (- 987654321 918273645) = 69380676 numbers to check, obviously too much to 
;; brute force, but not all are pandigital, which reduces the number of checks we 
;; have to do quite a bit:

;; (count (take-while #(< 918273645 %) perms))
;; 35899 

;; as the number of splits have to be more than one (n>1) the number of splits has 
;; to be 3
;; the number (map #(integer %) (partition 3 (digits 987654321)))

(defn check [[a b c]] (and (= (* 2 (integer a)) (integer b)) (= (* 3 (integer a)) (integer c))))
;;(filter #(check %) (map #(partition 3 (digits %)) (take-while #(< 918273645 %) perms)))
;; ()
;; so much for that train of thought
