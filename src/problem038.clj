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

(set! *print-length* 103)
(set! *print-level* 15)

(deftest test-examples
  (is (= 9 (count (reduce concat (map digits (map #(* 192 %) '(1 2 3)))))))
  (is (= 9 (count (reduce concat (map digits (map #(* 9 %) '(1 2 3 4 5))))))))

(defn pandigital? [number]
  (let [d (digits number)]
    (and (= 9 (count (distinct d)))
	 (= 9 (count d)))))

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
;;(filter check (map #(partition 3 (digits %)) (take-while #(< 918273645 %) perms)))
;; ()
;; so much for that train of thought

(def low-perms  (map integer (permutations '(1 2 3 4 5 6 7 8 9))))
;; (count  (take-while #(<= % 192384576) low-perms))
;;35378

;; (filter check (map #(partition 3 (digits %)) [192384576]))
;; (filter check (map #(partition 3 (digits %)) (take-while #(<= % 192384576 ) low-perms)))
;;(((1 9 2) (3 8 4) (5 7 6)))

;; (time (filter check (map #(partition 3 (digits %)) perms)))
;; (((3 2 7) (6 5 4) (9 8 1))  ((2 7 3) (5 4 6) (8 1 9))  ((2 1 9) (4 3 8) (6 5 7))  ((1 9 2) (3 8 4) (5 7 6)))

;
(def divs [100000000 10000000 1000000 100000 10000 1000 100 10 1])

(def perms  (map integer (permutations '(9 8 7 6 5 4 3 2 1))))
(def perms  (map integer (permutations '(9 8 7 6 5 4 3 2 1))))

;;(filter pandigital? (map #(+ (* % 1000000) (* 2 % 1000) (* 3 %)) (range 1 1000)))
;;(192384576 219438657 267534801 273546819 327654981 384769152 619237854 781562340 915830742 916832745)
(def divs [100000000 10000000 1000000 100000 10000 1000 100 10 1])
;          (10000             1000           100        10     1)

(filter pandigital? (map #(+ (* % 10000) (* 2 % 100) (* 3 %)) (range 1 100000)))
; 983742651

(filter pandigital? (map #(+ (* % 10000) (* 2 % 100) (* 3 %)) (range 1 100000)))

;; 2 cifre => 
(def d2 (distinct (map #(integer (take 2 %)) (permutations '(1 2 3 4 5 6 7 8 9)))))
;; 3 cifre =>
(def d3 (distinct (map #(integer (take 3 %)) (permutations '(1 2 3 4 5 6 7 8 9)))))

(defn task [p m n]
  (and (= (digits p) 
	  (reduce concat (map digits (map #(* m %) (range 1 (+ n 1)))))) 
       (pandigital? p)))

(defn check [n] 
  (filter #(task (integer %) (integer (take n %)) n) (permutations '(9 8 7 6 5 4 3 2 1))))
;; problem038> (check 3)
;; ((3 2 7 6 5 4 9 8 1) (2 7 3 5 4 6 8 1 9) (2 1 9 4 3 8 6 5 7) (1 9 2 3 8 4 5 7 6))
  
(def f
     (for [p [[3 2 7 6 5 4 9 8 1] [2 7 3 5 4 6 8 1 9]]]
       (for [n (range 1 9)] (list (integer p) n (integer (take n p))))))
(def g
     (for [p [[3 2 7 6 5 4 9 8 1] [2 7 3 5 4 6 8 1 9] [9 8 7 6 5 4 3 2 1]]
	   n (range 1 9)
	   :when (let [h (integer (take n p))
		       d (concat-numbers h (* h 2) (* h 3))] 
		   (different-digits? d))]

       (let [h (integer (take n p))
	     d (concat-numbers h (* h 2) (* h 3))] 
	 (list (integer p) n h d))))

;(reduce-while different-digits? concat-numbers  (map #(* 192 %) (range 1 10)))
;(reduce-while different-digits? concat-numbers (map #(* 192 %) (iterate inc 1)))

(defn reduce-while 
  "f is used on each item in coll until (pred (f(f(f item)))) fails"
  ([pred f coll]
     (let [s (seq coll)]
       (if  s
	 (reduce-while pred f (first s) (next s))
	 (f))))
  ([pred f val coll]
     (let [s (seq coll)]
       (if (and s (pred (f val (first s))))
	 (recur pred f (f val (first s)) (next s))
         val))))

(defn concat-numbers [& numbers]
  (integer (reduce concat (map #(digits %) numbers))))

(defn different-digits? [number]
  (let [d (digits number)]
    (= (count (distinct d)) (count d))))

(defn different-digits? [number]
  (let [d (digits number)]
    (and (every? #(not (zero? %)) d)
     (= (count (distinct d)) (count d)))))

;(reduce-while different-digits? concat-numbers (map #(* 192 %) (range 1 10)))

(defn check [number] 
  (reduce-while different-digits? concat-numbers (map (fn [n] (* number n)) (range 1 10))))

;; problem038> (filter #(< 123456789 %) (map #(check %) d3))
;; (327654981 273546819 267534801 219438657 192384576)
;; problem038> (filter #(< 123456789 %) (map #(check %) d2))
;; (1836547290)

;; take a permutation
;;  let each head partition be the current result (implicitly different digits)
;;  just take head partition concatenated with (* 2 head partition) 
;;    while the current result has different digits and current result <= 987654321
;;    multiply by n+1 and concatenate the result to the current result

;; problem038> (time (reduce max (filter #(< 123456789 %) (map #(check %) (range 1 100000)))))
;;"Elapsed time: 11738.315032 msecs"
;;932718654

(def d4  (distinct (map #(integer (take 4 %)) (permutations '(1 2 3 4 5 6 7 8 9)))))
(def d5  (distinct (map #(integer (take 5 %)) (permutations '(1 2 3 4 5 6 7 8 9)))))

;;(filter #(< 123456789 %) (map #(check %) d3))
;;(327654981 273546819 219438657 192384576)

(filter #(< 123456789 %) (map #(check %) d4))
(672913458 679213584 692713854 726914538 729314586 732914658 769215384 792315846 793215864 926718534 927318546 932718654)
;; BINGO 932718654


