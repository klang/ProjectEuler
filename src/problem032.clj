(ns problem032
  (meta {:description "We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, 39 x 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.

HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum."})
;;  #_(:use clojure.contrib.combinatorics
;;	clojure.contrib.math
;;	tools.numbers
;;	problem038
;;	clojure.test)
  (:use
   [clojure.test :only (deftest is)]
   [tools.numbers :only (digits integer)]
   [problem038 :only (concat-numbers)]
   [clojure.math.combinatorics :only (permutations) :as comb]))

(defn pandigital1-9? [number]
  (let [d (digits number)]
    (and (every? #(not (zero? %)) d)
	 (= 9 (count (distinct d)))
	 (= 9 (count d)))))


;; simple brute force, just to check if it is possible to do within the timelimit
(defn brute [n]
  (for [a (range 1 n) b (range a n) 
	:when 
	(and (not (= a b))
	     (pandigital1-9? (concat-numbers a b (* a b))))]
    {:a a :b b :prod (* a b)}))

(comment
  ({:a 12, :b 483, :prod 5796}   ; unique
   {:a 18, :b 297, :prod 5346}   ; double
   {:a 27, :b 198, :prod 5346} 
   {:a 28, :b 157, :prod 4396}   ; unique
   {:a 39, :b 186, :prod 7254}   ; unique
   {:a 42, :b 138, :prod 5796} 
   {:a 48, :b 159, :prod 7632})) ; unique

;; problem032> (time (reduce + (brute 700)))
;; "Elapsed time: 28664.974682 msecs"
;; 41566 -- wrong

;; problem032> (time (reduce + (into #{} (brute 500))))
;; "Elapsed time: 14110.260451 msecs"
;; 30424 -- wrong

;; something is missing.

;;  a is one digit, b must be in (range min max)
;; (map #(hash-map :a % :min (int (+ (/ 1000 %) 1)) :max (int (/ 10000 %))) (range 2 10))
;; for the concatenation of a b and (* a b) to be less than 9 digits

;; a is two digits, 
;;(map #(hash-map :a % :min (int (+ (/ 100 %) 1)) :max (int (/ 1000 %))) (range 10 100))
(defn clever []
  (into #{}
	(into 
	 ;; a x bbbb = cccc === aaaa x b = cccc
	 (for [a (range 2 10)
	       b (range (int (+ (/ 1000 a) 1)) (int (/ 10000 a)))
	       ;;:when
	       ;;(pandigital1-9? (concat-numbers a b (* a b)))
	       ] 
	   (list a b (* a b)))
	 ;; aa x bbb = cccc === aaa x bb = cccc
	 (for [a (range 100 1000)
	       b (range (int (+ (/ 1000 a) 1)) (int (/ 10000 a)))
	       :when
	       (pandigital1-9? (concat-numbers a b (* a b)))] (* a b)))))

;; problem032> (time (reduce + (clever)))
;; "Elapsed time: 2155.280704 msecs"
;; 30424

;; the problem with 'brute and 'clever is, that it takes way too long to find 
;; the 1,4,4 digit combination.

;; if a an b specifies the number of digits in two numbers
;; the result is the range of digits in the product of those numbers
(defn digits-in-product [a b]
  (range (max a b) (+ 1 (+ a b))))

;; (this has been moved, but was the first idea)
;; How many different places can a 'cut' be made in a known permutation of numbers
;; 1 2 3 4 5 6 7 8 9
;; 3 9 1 8 6 7 2 5 4
;;    ^     ^
;;    i     j
;; <--><----><----->
;;    i     j      k      
(def product-combinations
     (for [i (range 1 10) j (range i 10) k (digits-in-product i j)
	   :when (and  (< 0 i) (< i j) (= (+ i j k) 9) )] 
       {:i i :j j :k k}))

(defn s1 [i j]
  (filter #(= (* (integer (take i %)) 
		 (integer (take j (drop i %))))
	      (integer (drop (+ i j) %))) 
	  (permutations '(1 2 3 4 5 6 7 8 9))))

;; product-combinations
;; ({:i 1, :j 4, :k 4} {:i 2, :j 3, :k 4})
;; (s1 1 4)
;; ((4 1738 6952) (4 1963 7852))
;;  (s 2 3)
;; ((12 483 5796) (18 297 5346) (27 198 5346) (28 157 4396) 
;;  (39 186 7254 ) (42 138 5796) (48 159 7632))
;; 
;;(+ 6952 7852 5796 5346 4396 7254 7632)
;; 45228

;; problem032> (time (doall (brute 2000)))
;; "Elapsed time: 272892.293225 msecs"
;; (6952 7852 5796 5346 5346 4396 7254 5796 7632)
;; problem032> (reduce + (into #{} '( 6952 7852 5796 5346 5346 4396 7254 5796 7632)))
;; 45228

(defn problem032 [] nil)
