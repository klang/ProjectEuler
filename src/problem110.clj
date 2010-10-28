(ns problem110
  (meta
   {:description "In the following equation x, y, and n are positive integers.
1/x + 1/y = 1/n

For n = 4 there are exactly three distinct solutions:
1/5 + 1/20 = 1/6 + 1/12 = 1/8 + 1/8 = 1/4

What is the least value of n for which the number of distinct solutions exceeds four million?" 
    :note "There is a lot of comments in the following. They represent a lot of experimentation."})
  (:use [tools.primes :only (factors)])
  (:use [clojure.contrib.lazy-seqs :only (primes)])
  (:use [clojure.contrib.math :only (expt)])
  (:use [clojure.test :only (deftest is run-tests)])
  (:use [clojure.repl :only (dir)])
  )

(comment
  ;; not this one:
  (* 2 2 2 2 2 3 3 3 3 3 5 5 5 7 7 7 11 11 13 13 17 19 23)
  ;;2^5* 3^5 * 5^3 * 7^3 * 11^2 * 13^2 * 17 * 19 *23 = 50,648,060,378,916,000
  (frequencies [2 2 2 2 2 3 3 3 3 3 5 5 5 7 7 7 11 11 13 13 17 19 23])
  ;;{2 5, 3 5, 5 3, 7 3, 11 2, 13 2, 17 1, 19 1, 23 1}
  (reduce * (map (fn [[base pow]] (expt base pow))
		 (frequencies [ 2 2 2 2 2 3 3 3 3 3 5 5 5 7 7 7 11 11 13 13 17 19 23])))
  ;;50648060378916000
  ;; look at the end of the file for the definition of xsols..
  (xsols (* 2 2 2 2 3 3 3 3 5 5 7 7 11 13 (reduce * (take 9 primes))))
  ;; 2001038 solusions
  )

;; k distinct primes in the factorisation of n, multiplying by any prime in that list
;; increases the number of solutions of the equation by 3^k.
;; (actually, 3^(k-1))
(defn nsols [n]
  (count (filter #(zero? (rem (* n % ) (- % n))) (range (inc n) (inc (* 2 n))))))

(comment
  ;;deftest test-1-prime-repeated
  (is (= 27 (- (nsols (* 2 2 3 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 3 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 5 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 5 7 7)) (nsols (* 2 3 5 7))))
      (= (factors 27) [3 3 3]))
  (is (= 81 (- (nsols (* 2 2 3 5 7 11)) (nsols (* 2 3 5 7 11)))
	    (- (nsols (* 2 3 3 5 7 11)) (nsols (* 2 3 5 7 11)))
	    (- (nsols (* 2 3 5 5 7 11)) (nsols (* 2 3 5 7 11)))
	    (- (nsols (* 2 3 5 7 7 11)) (nsols (* 2 3 5 7 11)))
	    (- (nsols (* 2 3 5 7 11 11)) (nsols (* 2 3 5 7 11))))
      (= (factors 81) [3 3 3 3]))
  (is (= 243 (- (nsols (* 2 2 3 5 7 11 13)) (nsols (* 2 3 5 7 11 13))))
      (= (factors 243) [3 3 3 3 3]))
  (is (= 729 (- (nsols (* 2 2 3 5 7 11 13 17)) (nsols (* 2 3 5 7 11 13 17))))
      (= (factors 729) [3 3 3 3 3 3]))
  #_(is (= 2187 (- (nsols (* 2 2 3 5 7 11 13 17 19)) (nsols (* 2 3 5 7 11 13 17 19))))
	(= (factors 2187) [3 3 3 3 3 3 3]))
  )

(comment
  ;;deftest test-1-prime-repeated-twice
  (is (= 54 (- (nsols (* 2 2 2 3 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 3 3 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 5 5 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 5 7 7 7)) (nsols (* 2 3 5 7))))
      (= (factors 54) [2 3 3 3]))
  (is (= 162 (- (nsols (* 2 2 2 3 5 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 3 3 3 5 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 3 5 5 5 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 3 5 7 7 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 3 5 7 11 11 11)) (nsols (* 2 3 5 7 11))))
      (= (factors 162) [2 3 3 3 3]))
  (is (= 486 (- (nsols (* 2 2 2 3 5 7 11 13)) (nsols (* 2 3 5 7 11 13))))
      (= (factors 486) [2 3 3 3 3 3])))

(comment
  ;;deftest test-1-prime-repeated-3-times
  (is (= 81 (- (nsols (* 2 2 2 2 3 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 3 3 3 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 5 5 5 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 5 7 7 7 7)) (nsols (* 2 3 5 7))))
      (= (factors 81) [3 3 3 3]))
  (is (= 243 (- (nsols (* 2 2 2 2 3 5 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 3 3 3 3 5 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 3 5 5 5 5 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 3 5 7 7 7 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 3 5 7 11 11 11 11)) (nsols (* 2 3 5 7 11))))
      (= (factors 243) [3 3 3 3 3]))
  (is (= 729 (- (nsols (* 2 2 2 2 3 5 7 11 13)) (nsols (* 2 3 5 7 11 13))))
      (= (factors 729) [3 3 3 3 3 3])))

;; multiplying by two distinct primes from the list, increases the number of solusions of
;; the equation by 2^3 ? .. .. third by 7^2
;;(- (nsols (* 2 2 3 5 7)) (nsols (* 2 3 5 7)))

(defn factor1 [k] (expt 3 (- k 1)))
(deftest test-1-primes-repeated-formular
  (is (= 27 (factor1 4)))
  (is (= 81 (factor1 5)))
  (is (= 243 (factor1 6)))
  (is (= 729 (factor1 7)))
  (is (= 2187 (factor1 8))))

(comment
 ;;deftest test-2-primes-repeated
  ;; when having a distinct set of primes, multiplying with any of them, the 
  ;; number of solusions will go up by the same ammount.
  ;; for 1, that number is 3^(k-1) where k is the numer of distinct primes.
  ;;
  ;; 4 different factors - 2 distinct selected twice
  (is (= 72 (- (nsols (* 2 2 3 3 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 2 3 5 5 7)) (nsols (* 2 3 5 7)))))

  ;; 5 different factors 2^3 * 3^(k-2)
  ;; (k=5) 2 repeated
  (is (= 216 (- (nsols (* 2 2 3 3 5 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 2 3 5 5 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 2 3 5 7 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 2 3 5 7 11 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 2 3 5 7 7)) (nsols (* 2 3 5 7)))))

  (is (= 72 (- (nsols (* 2 2 3 3 5 7)) (nsols (* 2 3 5 7))))
      (= (factors 72) [2 2 2 3 3]))
  (is (= 216 (- (nsols (* 2 2 3 3 5 7 11)) (nsols (* 2 3 5 7 11))))
      (= (factors 216) [2 2 2 3 3 3]))
  (is (= 648 (- (nsols (* 2 2 3 3 5 7 11 13)) (nsols (* 2 3 5 7 11 13))))
      (= (factors 648) [2 2 2 3 3 3 3]))
  (is (= 1944 (- (nsols (* 2 2 3 3 5 7 11 13 17)) (nsols (* 2 3 5 7 11 13 17))))
      (= (factors 1944) [2 2 2 3 3 3 3 3]))
  #_(is (= 5832 (- (nsols (* 2 2 3 3 5 7 11 13 17 19)) (nsols (* 2 3 5 7 11 13 17 19))))
	(= (factors 5832) [2 2 2 3 3 3 3 3 3])))

(defn factor2 [k] (* (expt 2 3) (expt 3 (- k 2))))
(deftest test-2-primes-repeated
  (is (= 72 (factor2 4)))
  (is (= 216 (factor2 5)))
  (is (= 648 (factor2 6)))
  (is (= 1944 (factor2 7)))
  (is (= 5832 (factor2 8))))

(comment
  ;;deftest test-3-primes-repeated
  ;; (k=4) 3 repeated
  (is (= 147 (- (nsols (* 2 2 3 3 5 5 7)) (nsols (* 2 3 5 7)))
	     (- (nsols (* 2 2 3 3 5 7 7)) (nsols (* 2 3 5 7)))
	     (- (nsols (* 2 2 3 5 5 7 7)) (nsols (* 2 3 5 7)))
	     (- (nsols (* 2 3 3 5 5 7 7)) (nsols (* 2 3 5 7)))))

  ;; (k=5) 3 repeated
  (is (= 441 (- (nsols (* 2 2 3 3 5 5 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 2 3 5 5 7 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 2 3 3 5 7 7 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 2 3 3 5 7 11 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 2 3 5 7 7 11 11)) (nsols (* 2 3 5 7 11)))
	     (- (nsols (* 2 2 3 5 5 7 11 11)) (nsols (* 2 3 5 7 11)))))

  (is (= 147 (- (nsols (* 2 2 3 3 5 5 7)) (nsols (* 2 3 5 7))))
      (= (factors 147)) [3 7 7])
  (is (= 441 (- (nsols (* 2 2 3 3 5 5 7 11)) (nsols (* 2 3 5 7 11))))
      (=(factors 441) [3 3 7 7]))
  (is (= 1323 (- (nsols (* 2 2 3 3 5 5 7 11 13)) (nsols (* 2 3 5 7 11 13))))
      (= (factors 1323) [3 3 3 7 7]))
  (is (= 3969 (- (nsols (* 2 2 3 3 5 5 7 11 13 17)) (nsols (* 2 3 5 7 11 13 17))))
      (= (factors 3969) [3 3 3 3 7 7]))
  #_(is (= 11907 (-  (nsols (* 2 2 3 3 5 5 7 11 13 17 19)) (nsols (* 2 3 5 7 11 13 17 19))))
	(= (factors 11907) [3 3 3 3 37 7])))

(defn factor3 [k] (* (expt 7 2) (expt 3 (- k 3))))
(deftest test-3-primes-repeated-formular
  (is (= 147 (factor3 4)))
  (is (= 441 (factor3 5)))
  (is (= 1323 (factor3 6)))
  (is (= 3969 (factor3 7)))
  (is (= 11907 (factor3 8))))


(comment
  ;;deftest test-4-primes-repeated
  ;; (k=4) 4 repeated
  (is (= 272 (- (nsols (* 2 2 3 3 5 5 7 7)) (nsols (* 2 3 5 7))))
      (= (factors 272) [2 2 2 2 17]))
  ;; (k=5) 4 repeated
  #_(is (= 816 (- (nsols (* 2 2 3 3 5 5 7 7 11)) (nsols (* 2 3 5 7 11)))
	       (- (nsols (* 2 2 3 3 5 5 7 11 11)) (nsols (* 2 3 5 7 11)))
	       (- (nsols (* 2 2 3 3 5 7 7 11 11)) (nsols (* 2 3 5 7 11)))
	       (- (nsols (* 2 2 3 5 5 7 7 11 11)) (nsols (* 2 3 5 7 11)))
	       (- (nsols (* 2 3 3 5 5 7 7 11 11)) (nsols (* 2 3 5 7 11))))
	(= (factors 816) [2 2 2 2 3 17]))
  (is (= 2448 (- (nsols (* 2 2 3 3 5 5 7 7 11 13)) (nsols (* 2 3 5 7 11 13))))
      (= (factors 2448) [2 2 2 2 3 3 17]))
  (is (= 7344 (- (nsols (* 2 2 3 3 5 5 7 7 11 13 17)) (nsols (* 2 3 5 7 11 13 17))))
      (= (factors 7344) [2 2 2 2 3 3 3 17])))

(defn factor4 [k] (* (expt 2 4) 17 (expt 3 (- k 4))))
(deftest test-4-primes-repeated-formular
  (is (= 272 (factor4 4)))
  (is (= 816 (factor4 5)))
  (is (= 2448 (factor4 6)))
  (is (= 7344 (factor4 7))))

(comment
  ;;deftest test-5-primes-repeated
  (is (= 1441 (- (nsols (* 2 2 3 3 5 5 7 7 11 11)) (nsols (* 2 3 5 7 11))))
      (= (factors 1441) [11 131]))
  (is (= 4323 (- (nsols (* 2 2 3 3 5 5 7 7 11 11 13)) (nsols (* 2 3 5 7 11 13))))
      (= (factors 4323) [3 11 131]))
  ;; not tested..
  #_(is (= 12969 (- (nsols (* 2 2 3 3 5 5 7 7 11 11 13 17)) (nsols (* 2 3 5 7 11 13 17))))
	(= (factors 12969) [3 3 11 131])))

(defn factor5 [k] (* 11 131 (expt 3 (- k 5))))
(deftest test-5-primes-repeated
  (is (= 1441 (factor5 5)))
  (is (= 4323 (factor5 6)))
  (is (= 12969 (factor5 7))))

(comment
  ;;deftest test-2-primes-repeated-one-repeated-twice
  (is (= 117 (- (nsols (* 2 2 2 3 3 5 7)) (nsols (* 2 3 5 7 )))
	     (- (nsols (* 2 2 3 3 3 5 7)) (nsols (* 2 3 5 7 )))
	     (- (nsols (* 2 2 3 5 5 5 7)) (nsols (* 2 3 5 7 )))))
  (is (= 117 (- (nsols (* 2 2 2 3 3 5 7)) (nsols (* 2 3 5 7 )))
	     (- (nsols (* 2 2 3 3 3 5 7)) (nsols (* 2 3 5 7 )))
	     (- (nsols (* 2 2 3 5 5 5 7)) (nsols (* 2 3 5 7 ))))))

;;(first  (filter #(> (expt 3 %) 4000000 ) (iterate inc 1)))
;;14
;; .. so, we need at most 14 different primes
;; problem110> (take 14 primes)
;; (2 3 5 7 11 13 17 19 23 29 31 37 41 43)

;; 
;; To express myself somewhat more clear: 
;; As 3^7=2187 a sequence of 7 primefactors gives a solution although not a minimal one. 
;; So we start with 
;; 2*3*5*7*11*13*17. 
;; To reduce n we now try to replace one of the factors with a smaller one. Let's try to replace 17. Replacing 17 with one smaller primefactor won't do as 
;; 5*3*3*3*3*3=1215<2000. 
;; Also 7*3*3*3*3*3<2000 
;; But replacing 17 by a product of two sufficiently small different primefactors will do as 5*5*3*3*3*3=2025 
;; Then we have 
;; 2^2*3^2*5*7*11*13=180180. 
;; Is it possible to replace the 13 with a smaller factor? 
;; The only possiblities that we have to consider are: 
;; 12=2^2*3: 9*7*3*3*3=1701 
;; 10=2*5 : 7*5*5*3*3=1575 
;; 4 (or 9) =2^2 (and 3^2): 9*5*3*3*3=1215 
;; 8=2^3 :11*5*3*3*3=1485 
;; 6=2*3 :7*7*3*3*3=1323 
;; So it's not possible to get rid of the factor 13. 
;; But then 180180 is the minimal solution.

(def a126098 [1 2 4 6 12 24 30 60 120 180 210 360 420 840 1260 1680 2520 4620 7560 9240 13860 18480 27720 55440 
	      83160 110880 120120 180180 240240 360360 720720 1081080 1441440 1801800 2042040 2882880 3063060 
	      4084080 5405400 6126120 12252240 18378360 24504480])
;; (map #(factors %) a126098)
(comment
  ([] [2] [2 2] [2 3] [2 2 3] [2 2 2 3] [2 3 5] [2 2 3 5] [2 2 2 3 5] [2 2 3 3 5] [2 3 5 7] [2 2 2 3 3 5] [2 2 3 5 7] [2 2 2 3 5 7] [2 2 3 3 5 7] [2 2 2 2 3 5 7] [2 2 2 3 3 5 7] [2 2 3 5 7 11] [2 2 2 3 3 3 5 7] [2 2 2 3 5 7 11] [2 2 3 3 5 7 11] [2 2 2 2 3 5 7 11] [2 2 2 3 3 5 7 11] [2 2 2 2 3 3 5 7 11] [2 2 2 3 3 3 5 7 11] [2 2 2 2 2 3 3 5 7 11] [2 2 2 3 5 7 11 13] [2 2 3 3 5 7 11 13] [2 2 2 2 3 5 7 11 13] [2 2 2 3 3 5 7 11 13] [2 2 2 2 3 3 5 7 11 13] [2 2 2 3 3 3 5 7 11 13] [2 2 2 2 2 3 3 5 7 11 13] [2 2 2 3 3 5 5 7 11 13] [2 2 2 3 5 7 11 13 17] [2 2 2 2 2 2 3 3 5 7 11 13] [2 2 3 3 5 7 11 13 17] [2 2 2 2 3 5 7 11 13 17] [2 2 2 3 3 3 5 5 7 11 13] [2 2 2 3 3 5 7 11 13 17] [2 2 2 2 3 3 5 7 11 13 17] [2 2 2 3 3 3 5 7 11 13 17] [2 2 2 2 2 3 3 5 7 11 13 17]))

(comment
  (def d [(* 2 3 5)
	  (* 2 2 3 5) (* 2 3 3 5) (* 2 3 5 5)
	  (* 2 2 2 3 5) (* 2 2 3 3 5) (* 2 3 3 3 5) (* 2 2 3 5 5) (* 2 3 5 5 5) (* 2 2 3 3 5 5)])
  (map #(nsols %) d))

;; problem110> (time (nsols (* 2 3 )))
;; "Elapsed time: 0.342502 msecs"
;; 5
;; problem110> (time (nsols (* 2 3 5)))
;; "Elapsed time: 0.448382 msecs"
;; 14
;; problem110> (time (nsols (* 2 3 5 7)))
;; "Elapsed time: 0.929448 msecs"
;; 41
;; problem110> (time (nsols (* 2 3 5 7 11)))
;; "Elapsed time: 2.686376 msecs"
;; 122
;; problem110> (time (nsols (* 2 3 5 7 11 13)))
;; "Elapsed time: 47.794363 msecs"
;; 365
;; problem110> (time (nsols (* 2 3 5 7 11 13 17)))
;; "Elapsed time: 599.469696 msecs"
;; 1094
;; problem110> (time (nsols (* 2 3 5 7 11 13 17 19)))
;; "Elapsed time: 11389.4075 msecs"
;; 3281
;; problem110> (time (nsols (* 2 3 5 7 11 13 17 19 23)))
;; "Elapsed time: 265600.179022 msecs"
;; 9842

;;
;; a certain number of different primes always yield a specific number of different solusions?
;;
;; (/ (- (expt 3 5) 1) 2)
(defn sols
  "k specifies the number of different primes in the factorisation of a number"
  [k]
  (quot (+ (expt 3 k) 1) 2)) 
  
(deftest test-number-of-primes-related-to-number-of-solusions
  (is (=  41 (sols 4) (nsols (* 2 3 5 7)) (nsols (* 2 3 5 11)) (nsols (* 2 3 5 13)) (nsols (* 2 3 5 97)))))
 
;; problem110> (time (nsols (* 2 3 5 7)))
;; "Elapsed time: 0.93308 msecs"
;; 41
;; problem110> (time (nsols (* 2 3 5 7 11)))
;; "Elapsed time: 2.662071 msecs"
;; 122
;; problem110> (time (nsols (* 2 3 5 7 13)))
;; "Elapsed time: 14.475309 msecs"
;; 122
;; problem110> (time (nsols (* 2 3 5 7 23)))
;; "Elapsed time: 10.938266 msecs"
;; 122
;; problem110> (time (nsols (* 2 3 5 7 19)))
;; "Elapsed time: 4.468167 msecs"
;; 122
;; problem110> (time (nsols (* 2 3 5 19)))
;; "Elapsed time: 0.835023 msecs"
;; 41
;; problem110> (time (nsols (* 2 3 5 19 23)))
;; "Elapsed time: 14.299028 msecs"
;; 122
(defn sols "k specifies the number of different primes in the factorisation of a number"
  [k]
  (quot (+ (expt 3 k) 1) 2))

;; [2 1 1 ...] [1 2 1 ...] [1 1 2  ...]
(defn factor1 "one factor repeated once"   [k] (expt 3 (- k 1)))
;; [2 2 1 ...] [1 2 2 ...] [2 1 2  ...]
(defn factor2 "two factors repeated once"  [k] (* (expt 2 3) (expt 3 (- k 2))))
(defn factor3 "tree factors repeated once" [k] (* (expt 7 2) (expt 3 (- k 3))))
(defn factor4 "four factors repeated once" [k] (* (expt 2 4) 17 (expt 3 (- k 4))))
(defn factor5 "five factors repeated once" [k] (* 11 131 (expt 3 (- k 5))))

;; [3 1 1 ...] [1 3 1 ...] [1 1 3  ...]
;; [4 1 1 ...] [1 4 1 ...] [1 1 4  ...]
;; [5 1 1 ...] [1 5 1 ...] [1 1 5  ...]

(comment
  ;;deftest checking-how-the-differences-change-if-factors-go-above-2
  ;; at least it seems there are formulars guarding the results
  (is (= 8 (- (nsols (* 2 2 3 3)) (nsols (* 2 3))))
      (= (factors 8) [2 2 2]))
  (is (= 24 (- (nsols (* 2 2 3 3 5)) (nsols (* 2 3 5)))
	    (- (nsols (* 2 2 3 5 5)) (nsols (* 2 3 5)))
	    (- (nsols (* 2 3 3 5 5)) (nsols (* 2 3 5))))
      (= (factors 24) [2 2 2 3]))
  (is (= 72 (- (nsols (* 2 2 3 3 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 2 3 5 5 7)) (nsols (* 2 3 5 7)))
	    (- (nsols (* 2 3 3 5 5 7)) (nsols (* 2 3 5 7))))
      (= (factors 72) [2 2 2 3 3]))
  ;;-----
  (is (= 13 (- (nsols (* 2 2 2 3 3)) (nsols (* 2 3)))
	    (- (nsols (* 2 2 3 3 3)) (nsols (* 2 3))))
      (= (factors 13) [13]))
  (is (= 39 (- (nsols (* 2 2 2 3 3 5)) (nsols (* 2 3 5)))
	    (- (nsols (* 2 2 3 3 3 5)) (nsols (* 2 3 5)))
	    (- (nsols (* 2 2 3 5 5 5)) (nsols (* 2 3 5)))
	    (- (nsols (* 2 2 3 5 5 5)) (nsols (* 2 3 5))))
      (= (factors 39) [3 13]))
  (is (= 117 (- (nsols (* 2 2 2 3 3 5 7)) (nsols (* 2 3 5 7 )))
	     (- (nsols (* 2 2 3 3 3 5 7)) (nsols (* 2 3 5 7 )))
	     (- (nsols (* 2 2 3 5 5 5 7)) (nsols (* 2 3 5 7 )))
	     (- (nsols (* 2 2 3 5 5 5 7)) (nsols (* 2 3 5 7 ))))
      (= (factors 117) [3 3 13]))
  (is (= 351 (- (nsols (* 2 2 3 3 3 5 7 11)) (nsols (* 2 3 5 7 11))))
      (= (factors 351) [3 3 3 13]))
  ;;----
  (is (= 222 (- (nsols (* 2 2 3 3 5 5 5 7)) (nsols (* 2 3 5 7 ))))
      (= (factors 222) [2 3 37]))
  (is (= 74 (- (nsols (* 2 2 3 3 5 5 5)) (nsols (* 2 3 5))))
      (= (factors 74) [2 37]))
  ;;----
  (is (= 49 (- (nsols (* 2 2 3 3 5 5)) (nsols (* 2 3 5 ))))
      (= (factors 49) [7 7]))
  (is (= 147 (- (nsols (* 2 2 3 3 5 5 7)) (nsols (* 2 3 5 7 ))))
      (= (factors 147) [3 7 7]))
  ;;---
  (is (= 162 (- (nsols (* 2 2 3 5 5 5 5 7)) (nsols (* 2 3 5 7 ))))
      (= (factors 162) [2 3 3 3 3]))

  (is (= 162 (- (nsols (* 2 2 3 5 5 5 5 7)) (nsols (* 2 3 5 7 ))))
      (= (factors 162) [2 3 3 3 3]))
  (is (= 486 (- (nsols (* 2 2 3 5 5 5 5 7 11)) (nsols (* 2 3 5 7 11)))))
  ;;----
  (is (= 272 (- (nsols (* 2 2 3 3 5 5 7 7)) (nsols (* 2 3 5 7))))
      (= (factors 272) [2 2 2 2 17]))
  (is (= 816 (- (nsols (* 2 2 3 3 5 5 7 7 11)) (nsols (* 2 3 5 7 11))))
      (= (factors 816) [2 2 2 2 3 17]))


  (is (= 9 (- (nsols (* 2 2 2 2 3)) (nsols (* 2 3))) (- (nsols (* 2 3 3 3 3)) (nsols (* 2 3))))
      (= (factors 9) [3 3]))
  (is (= 9 (- (nsols (* 2 2 2 2 3)) (nsols (* 2 3))) (- (nsols (* 2 3 3 3 3)) (nsols (* 2 3))))
      (= (factors 9) [3 3]))
  (is (= 9 (- (nsols (* 2 2 2 2 3)) (nsols (* 2 3))) (- (nsols (* 2 3 3 3 3)) (nsols (* 2 3))))
      (= (factors 9) [3 3]))

  (is (= 12 (- (nsols (* 2 2 2 2 2 3)) (nsols (* 2 3))) (- (nsols (* 2 3 3 3 3 3)) (nsols (* 2 3))))
      (= (factors 12) [2 2 3]))
  (is (= 15 (- (nsols (* 2 2 2 2 2 2 3)) (nsols (* 2 3))) (- (nsols (* 2 3 3 3 3 3 3 )) (nsols (* 2 3))))
      (= (factors 15) [3 5]))
  (is (= 18 (- (nsols (* 2 2 2 2 2 2 2 3)) (nsols (* 2 3))) (- (nsols (* 2 3 3 3 3 3 3 3 )) (nsols (* 2 3))))
      (= (factors 18) [2 3 3]))
  
  ;;-- two factors equal
  
  (is (= 8 (- (nsols (* 2 2 3 3)) (nsols (* 2 3))) )
      (= (factors 8) [2 2 2]))
  (is (= 24 (- (nsols (* 2 2 3 3 5)) (nsols (* 2 3 5))) )
      (= (factors 24) [2 2 2 3]))
  (is (= 20 (- (nsols (* 2 2 2 3 3 3)) (nsols (* 2 3))) )
      (= (factors 20) [2 2 5]))
  (is (= 36 (- (nsols (* 2 2 2 2 3 3 3 3)) (nsols (* 2 3))))
      (= (factors 36) [2 2 3 3]))
  (is (= 56 (- (nsols (* 2 2 2 2 2 3 3 3 3 3)) (nsols (* 2 3))))
      (= (factors 56) [2 2 2 7]))
  (is (= 80 (- (nsols (* 2 2 2 2 2 2 3 3 3 3 3 3 )) (nsols (* 2 3))))
      (= (factors 80) [2 2 2 2 5]))
  (is (= 108 (- (nsols (* 2 2 2 2 2 2 2 3 3 3 3 3 3 3)) (nsols (* 2 3))))
      (= (factors 108) [2 2 3 3 3]))
)
  

(defn factor-2-2-1-1-1 "(* f1^2 f2^2 f3 f4 ... fk)" [k] (* 2 2 2 (expt 3 (- k 2))))
(defn factor-3-2-1-1-1 "(* f1^3 f2^2 f3 f4 ... fk)" [k] (* 13 (expt 3 (- k 2))))
(defn factor-2-2-2-1-1 "(* f1^2 f2^2 f3^2 f4 ... fk)" [k] (* 7 7 (expt 3 (- k 3))))
(defn factor-3-2-2-1-1 "(* f1^3 f2^2 f3^2 f4 ... fk)" [k] (* 2 37 (expt 3 (- k 3))))
(defn factor-2-2-2-2-1 "(* f1^2 f2^2 f3^2 f4^2 ... fk)" [k] (* 2 2 2 2 17 (expt 3 (- k 4))))


;; k number of different factors
;; r number of factors repeated
(defn factor-2-2-2-2-2 [k r] (* (reduce * (factors
					   (- (nsols (reduce * (take (* 2 r) (cycle (take r primes)))))
					      (sols r))))
				(expt 3 (- k r))))

;; (take (* 2 r) (cycle (take r primes)))
;; a fancy way of writing first r primes to the second power (the smallest number with nsols)

;;does this make basis for a general formular?

(deftest test-complicated-formulars
  (is (= 8 (factor-2-2-1-1-1 2)))
  (is (= 24 (factor-2-2-1-1-1 3)))
  (is (= 72 (factor-2-2-1-1-1 4)))
  
  (is (= 13 (factor-3-2-1-1-1 2)))
  (is (= 39 (factor-3-2-1-1-1 3)))
  (is (= 117 (factor-3-2-1-1-1 4)))

  (is (= 49 (factor-2-2-2-1-1 3)))
  (is (= 147 (factor-2-2-2-1-1 4)))

  (is (= 74 (factor-3-2-2-1-1 3))
      (= 222 (factor-3-2-2-1-1 4)))
  
  (is (= 272 (factor-2-2-2-2-1 4))
      (= 816 (factor-2-2-2-2-1 5)))
    (is (= 272 (factor-2-2-2-2-2 4 4))
	(= 816 (factor-2-2-2-2-2 5 4)))
  
  )

(comment
  (+  (sols 14) (factor1 14))
  ;; 3985808
  (+  (sols 14) (factor1 14))
  (+  (sols 14) (factor2 14))
  ;; 6643013
  (+  (sols 13) (factor1 13))
  ;; 1328603
  (+  (sols 13) (factor2 13))
  ;; 2214338
  (+  (sols 13) (factor3 13))
  ;; 3690563
  (+  (sols 13) (factor4 13))
  ;; 6150938

  (+  (sols 12) (factor4 12))
  ;; 2050313
  (+  (sols 12) (factor5 12))
  ;; 3417188
  )

(comment
  ;;deftest test-one-power-going-up
  ;; f1^j ...
  (let [nsols23 (nsols (* 2 3))
	base (* 2 2 3)
	case [base (* 2 base) (* 2 2 base) (* 2 2 2 base) (* 2 2 2 2 base) (* 2 2 2 2 2 base)]]
    (is (= (map #(- (nsols %) nsols23) case)
	   (map #(+ 3 (* 3 %)) [0 1 2 3 4 5])
	   [3 6 9 12 15 18])))
  (let [nsols23 (nsols (* 2 3))
	base (* 2 3 3)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3) (* base 3 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols23) case)
	   (map #(+ 3 (* 3 %)) [0 1 2 3 4 5])
	   [3 6 9 12 15 18]))))

(comment
  ;; deftest test-two-powers-going-up
  ;; general formular:
  (map #(list (+ 0 (* 3 %)) (* (+ 3 (* 2 %)))) [0 1 2 3 4 5])
  ;; ((0 3) (3 5) (6 7) (9 9) (12 11) (15 13))
  ;; one power locked (by position in sequence) one power variable in j,
  ;; result should be multiplied by (expt 3 (- k 2)), with k, the number of different primes
  (def formular1 (map #(fn [j] (+ (+ 0 (* 3 %)) (* (+ 3 (* 2 %)) j))) (iterate inc 0)))
  ;; ((nth formular1 n) j) == f1^(n+1) f2^(j+1)
  ;; i.e. ((nth formular1 1) 1) =  f1^2 f2^2

    ;; f1^1 f2^j ...   (+ 0 (* 3 j)) ==> (nth formular 0)
  (let [nsols23 (nsols (* 2 3))
	  base (* 2 3)
	  case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols23) case)
	   (map #(+ 0 (* 3 %)) [0 1 2 3 4])
	   (map #((nth formular1 0) %) [0 1 2 3 4])
	   [0 3 6 9 12])))

  ;; f1^2 f2^j ...   (+ 3 (* 5 j)) ==> (nth formular 1)
  (let [nsols23 (nsols (* 2 3))
	  base (* 2 2 3)
	  case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols23) case)
	   (map #(+ 3 (* 5 %)) [0 1 2 3 4])
	   (map #((nth formular1 1) %) [0 1 2 3 4])
	   [3 8 13 18 23])))
  
  ;; f1^3 f2^j ...   (+ 6 (* 7 j)) ==> (nth formular 2)
  (let [nsols23 (nsols (* 2 3))
	base (* 2 2 2 3)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols23) case)
	   (map #(+ 6 (* 7 %)) [0 1 2 3 4])
	   (map #((nth formular1 2) %) [0 1 2 3 4])
	   [6 13 20 27 34])))

  ;; f1^4 f2^j ...   (+ 9 (* 9 j)) ==> (nth formular 3)
  (let [nsols23 (nsols (* 2 3))
	base    (* 2 2 2 2 3)
	case     [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols23) case)
	   (map #(+ 9 (* 9 %)) [0 1 2 3 4])
	   (map #((nth formular1 3) %) [0 1 2 3 4])
	   [9 18 27 36 45])))

  ;; f1^5 f2^j ...   (+ 12 (* 11 j)) ==> (nth formular 4)
  (let [nsols23 (nsols (* 2 3))
	base    (* 2 2 2 2 2 3)
	case     [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols23) case)
	   (map #(+ 12 (* 11 %)) [0 1 2 3 4])
	   (map #((nth formular1 4) %) [0 1 2 3 4])
	   [12 23 34 45 56])))

  ;; f1^6 f2^j ...   (+ 15 (* 13 j)) ==> (nth formular 5)
  (let [nsols23 (nsols (* 2 3))
	base    (* 2 2 2 2 2 2 3)
	case     [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols23) case)
	   (map #(+ 15 (* 13 %)) [0 1 2 3 4])
	   (map #((nth formular1 5) %) [0 1 2 3 4])
	   [15 28 41 54 67])))
  )

;;  (* ((nth formular 0) 2) 3) == f1^1 f2^i f3 .. as expected
;;  (* ((nth formular 0) 2) (expt 3 (- k 2) == f1^1 f2^i f3 .. fk 
(comment
  ;;deftest test-two-powers-going-up-with-3-distinct-primes 
  ;; f1^1 f2^j ...   (+ 0 (* 3 j)) ==> (nth formular 0)
  (let [nsols235 (nsols (* 2 3 5))
	base (* 2 3 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 0 (* 9 %)) [0 1 2 3 4])
	   (map #(* (expt 3 1) ((nth formular1 0) %)) [0 1 2 3 4])
	   [0 9 18 27 36])))

  ;; f1^2 f2^j ...   (+ 3 (* 5 j)) ==> (nth formular 1)
  (let [nsols235 (nsols (* 2 3 5))
	  base (* 2 2 3 5)
	  case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 9 (* 15 %)) [0 1 2 3 4])
	   (map #(* (expt 3 1) ((nth formular1 1) %)) [0 1 2 3 4])
	   [9 24 39 54 69])))
  
  ;; f1^3 f2^j ...   (+ 6 (* 7 j)) ==> (nth formular 2)
  (let [nsols235 (nsols (* 2 3 5))
	base (* 2 2 2 3 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 18 (* 21 %)) [0 1 2 3 4])
	   (map #(* (expt 3 1) ((nth formular1 2) %)) [0 1 2 3 4])
	   [18 39 60 81 102])))

  ;; f1^4 f2^j ...   (+ 9 (* 9 j)) ==> (nth formular 3)
  (let [nsols235 (nsols (* 2 3 5))
	base    (* 2 2 2 2 3 5)
	case     [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 27 (* 27 %)) [0 1 2 3 4])
	   (map #(* (expt 3 1) ((nth formular1 3) %)) [0 1 2 3 4])
	   [27 54 81 108 135])))

  ;; f1^5 f2^j ...   (+ 12 (* 11 j)) ==> (nth formular 4)
  (let [nsols235 (nsols (* 2 3 5))
	base    (* 2 2 2 2 2 3 5)
	case     [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 36 (* 33 %)) [0 1 2 3 4])
	   (map #(* (expt 3 1) ((nth formular1 4) %)) [0 1 2 3 4])
	   [36 69 102 135 168])))

  ;; f1^6 f2^j ...   (+ 15 (* 13 j)) ==> (nth formular 5)
  (let [nsols235 (nsols (* 2 3 5))
	base    (* 2 2 2 2 2 2 3 5)
	case     [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 45 (* 39 %)) [0 1 2 3 4])
	   (map #(* (expt 3 1) ((nth formular1 5) %)) [0 1 2 3 4])
	   [45 84 123 162 201])))
  )
  
(comment
  ;; some of these test take a while to run
  (map #(list (+ 18 (* 21 %)) (* (+ 21 (* 14 %)))) [0 1 2 3 4 5])
  ;; ((18 21) (39 35) (60 49) (81 63))

  ;; one power locked (by position in sequence) one power variable in j,

  ;; result should be multiplied by (expt 3 (- k 2)), with k, the number of different primes
  (def formular2 (map #(fn [j] (+ (+ 18 (* 21 %)) (* (+ 21 (* 14 %)) j))) (iterate inc 0)))
  ;; ((nth formular 0) 2) == f1^3 f2^j
  ;;                ^f1^(n-1)

  (let [nsols235 (nsols (* 2 3 5))
	base (* 2 2 2 3 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 18 (* 21 %)) [0 1 2 3 4])
	   [18 39 60 81 102])))

  (let [nsols235 (nsols (* 2 3 5))
	base (* 2 2 2 3 5 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 39 (* 35 %)) [0 1 2 3 4])
	   [39 74 109 144 179])))

  (let [nsols235 (nsols (* 2 3 5))
	base (* 2 2 2 3 5 5 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 60 (* 49 %)) [0 1 2 3 4])
	   [60 109 158 207 256])))

  (let [nsols235 (nsols (* 2 3 5))
	base (* 2 2 2 3 5 5 5 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 81 (* 63 %)) [0 1 2 3 4])
	   [81 144 207 270 333])))
  )

;;TODO: multiply by the number of 'single instance' primes
;;TODO: describe each of these formulars
;; ((nth formular1 0) 2) == f1^1 f2^j
;;                 ^f1^(p-1), p is the power one prime, j is the power of another
;; ((nth formular1 n) j) == f1^(n+1) f2^(j+1)
;; i.e. ((nth formular1 1) 1) =  f1^2 f2^2
(def formular1 (map #(fn [j] (+ (+ 0 (* 3 %)) (* (+ 3 (* 2 %)) j))) (iterate inc 0)))

;; ((nth formular2 0) 2) == f1^3 f2^(n-1) f3^j
;;                 ^f2^(p-1)
(def formular2 (map #(fn [j] (+ (+ 18 (* 21 %)) (* (+ 21 (* 14 %)) j))) (iterate inc 0)))

;; ((nth formular2 0) 2) == f1^4 f2^(n-1) f3^j
;;                 ^f2^(p-1)
;; for all the formulars, f1,f2,f3,fm specify arbitrary primes
(def formular3 (map #(fn [j] (+ (+ 27 (* 27 %)) (* (+ 27 (* 18 %)) j))) (iterate inc 0)))

;;
(comment
  (map #(list (+ 0 (* 3 %)) (* (+ 3 (* 2 %)))) [0 1 2 3 4 5])
  ;; ((0 3) (3 5) (6 7) (9 9) (12 11) (15 13))
  (map #(list (+ 18 (* 21 %)) (* (+ 21 (* 14 %)))) [0 1 2 3 4 5])
  ;; ((18 21) (39 35) (60 49) (81 63) (102 77) (123 91))
  (map #(list (+ 27 (* 27 %)) (* (+ 27 (* 18 %)))) [0 1 2 3 4 5])
  ;; ((27 27) (54 45) (81 63) (108 81) (135 99) (162 117))

  (let [nsols235 (nsols (* 2 3))
	base0 (* 2 3)
	base1 (* base0 2)
	case0 [base0 (* base0 3)]
	case1 [base1 (* base1 3)]
	s1 (map #(- (nsols %) nsols235) case0)
	s2 (map #(- (nsols %) nsols235) case1)]
    {:results (list s1 s2)
     :sum (list (first s1) (- (second s1) (first s1)))
     :mul (list (- (first s2) (first s1)) (- (- (second s2) (second s1))
					     (- (first s2) (first s1))))
     :exp (list '(0 3) '(3 2))})
  
  (let [nsols235 (nsols (* 2 3 5))
	base0 (* 2 2 2 3 5)
	base1 (* base0 5)
	case0 [base0 (* base0 3)]
	case1 [base1 (* base1 3)]
	s1 (map #(- (nsols %) nsols235) case0)
	s2 (map #(- (nsols %) nsols235) case1)]
    {:results (list s1 s2)
     :sum (list (first s1) (- (second s1) (first s1)))
     :mul (list (- (first s2) (first s1)) (- (- (second s2) (second s1))
					     (- (first s2) (first s1))))
     :exp (list '(18 21) '(21 14))})

  (let [nsols235 (nsols (* 2 3 5))
	base0 (* 2 2 2 2 3 5)
	base1 (* base0 5)
	case0 [base0 (* base0 3)]
	case1 [base1 (* base1 3)]
	s1 (map #(- (nsols %) nsols235) case0)
	s2 (map #(- (nsols %) nsols235) case1)]
    {:results (list s1 s2)
     :sum (list (first s1) (- (second s1) (first s1)))
     :mul (list (- (first s2) (first s1)) (- (- (second s2) (second s1))
					     (- (first s2) (first s1))))
     :exp (list '(27 27) '(27 18))})
  )

(defn formular-producer
  "calculating number of extra solusions when base is multiplied to the
  sample, a general formular is produced."
  [sample base p1 p2]
  (assert (<= sample base))
  (let [nsols235 (nsols sample)
	base0 base
	base1 (* base0 p1)
	case0 [base0 (* base0 p2)]
	case1 [base1 (* base1 p2)]
	s1 (map #(- (nsols %) nsols235) case0)
	s2 (map #(- (nsols %) nsols235) case1)
	f {:results (list s1 s2)
	   :formular (assoc (frequencies (factors base)) p1 :j p2 :n)
	   :a (first s1)
	   :b (- (second s1) (first s1))
	   :c (- (first s2) (first s1))
	   :d (- (- (second s2) (second s1)) (- (first s2) (first s1)))
	   }]
    (do (println f))
    (map #(fn [j] (+ (+ (f :a) (* (f :b) %)) (* (+ (f :c) (* (f :d) %)) j))) (iterate inc 0))))

(defn formular-calculator
  "calculating number of extra solusions when base is multiplied to the
  sample, a general formular is produced."
  [sample base p1 p2]
  (assert (<= sample base))
  (let [nsols235 (nsols sample)
	base0 base
	base1 (* base0 p1)
	case0 [base0 (* base0 p2)]
	case1 [base1 (* base1 p2)]
	s1 (map #(- (nsols %) nsols235) case0)
	s2 (map #(- (nsols %) nsols235) case1)]
    #_(do (println {:results (list s1 s2)
		    :formular (assoc (frequencies (factors base)) p1 :j p2 :n)}))
    [(first s1)
     (- (second s1) (first s1))
     (- (first s2) (first s1))
     (- (- (second s2) (second s1)) (- (first s2) (first s1)))]))

(defn formular-builder [[a b c d]]
  (map #(fn [j] (+ (+ a (* b %)) (* (+ c (* d %)) j))) (iterate inc 0)))

(defn formular-producer [sample base p1 p2]
  (formular-builder (formular-calculator sample base p1 p2)))

;; formular1: 2^(n+1), 3^j --> ((nth f1 
(def f1a (formular-producer (* 2 3) (* 2 3) 2 3))
(def f1b (formular-producer (* 2 3) (* 2 3) 3 2))
(def f-p1 (formular-producer (* 2 3 5) (* 2 3 5) 3 5)) 
(def f-p1 (formular-producer (* 2 3) (* 2 3) 3 2)) ;; if multiplied by (expt 3 4) and not (expt 3 3)
;; missing: {:results ((9 24) (24 49)), :a 9, :b 15, :c 15, :d 10}
;; formular-p2 for 2^2, 3^(n+1), 5^j
(def f-p2 (formular-producer (* 2 3 5) (* 2 2 3 5) 5 3))
;; formular2: 2^3, 3^(n+1), 5^j --> ((nth f2 n) j)
;; formular-p3
(def f2a (formular-producer (* 2 3 5) (* 2 2 2 3 5) 5 3))
(def f2b (formular-producer (* 2 3 5) (* 2 2 2 3 5) 3 5))
(def f-p3 (formular-producer (* 2 3 5) (* 2 2 2 3 5) 3 5))
;; formular3 2^4, 3^(n+1), 5^j --> ((nth f3 n) j)
;; formular-p4
(def f3a (formular-producer (* 2 3 5) (* 2 2 2 2 3 5) 5 3))
(def f3b (formular-producer (* 2 3 5) (* 2 2 2 2 3 5) 3 5))
(def f-p4 (formular-producer (* 2 3 5) (* 2 2 2 2 3 5) 5 3))

(deftest test-formular-producer
  (is (= ((nth formular1 1) 1) ((nth f1b 1) 1) ((nth f1a 1) 1) ((nth f-p1 1) 1)))
  (is (= #_(comment "not really checked!! but it works!") 49 ((nth f-p2 1) 1)))
  (is (= ((nth formular2 1) 1) ((nth f2b 1) 1) ((nth f2a 1) 1) ((nth f-p3 1) 1)))
  (is (= ((nth formular3 1) 1) ((nth f3b 1) 1) ((nth f3a 1) 1) ((nth f-p4 1) 1)))
  (is (= 1013 #_(nsols (* 2 2 3 3 5 7 11 13))            ;; p1^2 p2^2 p3 p4 p5 p6
	 (+ (sols 6) (* (expt 3 3) ((nth f-p1 1) 1)))))
  (is (= 1688 #_(nsols (* 2 2 3 3 5 5 7 11 13))          ;; p1^2 p2^2 p3^2 p4 p5 p6
	 (+ (sols 6) (* (expt 3 3) ((nth f-p2 1) 1)))))
  (is (= 2363 #_(nsols (* 2 2 2 3 3 5 5 7 11 13))        ;; p1^3 p2^2 p3^2 p4 p5 p6
	 (+ (sols 6) (* (expt 3 3) ((nth f-p3 1) 1)))))
  (is (= 3038 #_(nsols (* 2 2 2 2 3 3 5 5 7 11 13))      ;; p1^4 p2^2 p3^2 p4 p5 p6
	 (+ (sols 6) (* (expt 3 3) ((nth f-p4 1) 1)))))
  )

(def f-p1 (formular-producer (* 2 3 5) (* 2 3 5) 5 3))
(def f-p2 (formular-producer (* 2 3 5) (* 2 2 3 5) 5 3))
(def f-p3 (formular-producer (* 2 3 5) (* 2 2 2 3 5) 5 3))
(def f-p4 (formular-producer (* 2 3 5) (* 2 2 2 2 3 5) 5 3))
(def f-p5 (formular-producer (* 2 3 5) (* 2 2 2 2 2 3 5) 5 3))

(defn f-p [m]
  (formular-producer (* 2 3 5) (* (reduce * (repeat m 2)) 3 5) 3 5))
(deftest test-f-p
  (is (= ((nth (f-p 1) 1) 1) ((nth f-p1 1) 1)))
  (is (= ((nth (f-p 1) 2) 3) ((nth f-p1 2) 3)))
  (is (= ((nth (f-p 2) 1) 1) ((nth f-p2 1) 1)))
  (is (= ((nth (f-p 3) 1) 1) ((nth f-p3 1) 1)))
  (is (= ((nth (f-p 4) 1) 1) ((nth f-p4 1) 1)))
  (is (= ((nth (f-p 5) 1) 1) ((nth f-p5 1) 1)))
  (is (= ((nth (f-p 5) 7) 4) ((nth f-p5 7) 4)))
  )

(defn dsols [k]
  (let [ps (drop 3 (take k primes))
	solsk (sols k)
	expt3k3 (expt 3 (- k 3))]
    (for [m (range 1 18) j (range 0 20) n (range 0 20) :when (and (<= n j) (< j m))]
      {:sols (+ solsk (* expt3k3 ((nth (f-p m) j) n)))
       :2 m :3 (inc j) :5 (inc n)
       :n (reduce * (flatten (merge (repeat m 2)
				    (repeat (inc j) 3)
				    (repeat (inc n) 5) ps)))
       })))



;; (count (for [m (range 1 15) j (range 0 15) n (range 0 10) :when (and (<= n j) (< j m))] 1))
;; 540
;; (count (for [m (range 1 18) j (range 0 15) n (range 0 16) :when (and (<= n j) (< j m))] 1))
;; 540
(def f-p (memoize f-p))
(comment
  (filter #(< 4000000 (:sols %) 4400000) (dsols 11))
  ({:sols 4222004, :2 6, :3 5, :5 4, :n 64981598802120000}
   {:sols 4294175, :2 8, :3 5, :5 3, :n 51985279041696000}
   {:sols 4182638, :2 8, :3 7, :5 2, :n 93573502275052800}
   {:sols 4051418, :2 9, :3 6, :5 2, :n 62382334850035200}))

;; {:sols 4018613, :2 12, :3 3, :5 3, :n 92418273851904000}
;;problem110> (filter #(< 4000000 (:sols %) 4400000) (dsols 11))
;;({:sols 4222004, :2 6, :3 5, :5 4, :n 64981598802120000})
(comment
  {:sols 4018613, :2 12, :3 3, :5 3, :n 92418273851904000} ;; not the correct answer
  {:sols 4018613, :2 17, :3 3, :5 2, :n 591476952652185600})

(comment
  (map #(list (+ 27 (* 27 %)) (* (+ 27 (* 18 %)))) [0 1 2 3 4 5])
  ;;((27 27) (54 45) (81 63) (108 81) (135 99) (162 117))
  (def formular3 (map #(fn [j] (+ (+ 27 (* 27 %)) (* (+ 27 (* 18 %)) j))) (iterate inc 0)))

  (defn formular [start step]
    (map #(fn [j] (+ (+ 27 (* 27 %)) (* (+ 27 (* 18 %)) j))) (iterate inc 0)))

    ;; some of these tests take a while to run
  (let [nsols235 (nsols (* 2 3 5))
	base (* 2 2 2 2 3 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 27 (* 27 %)) [0 1 2 3 4])
	   [27 54 81 108 135])))

    (let [nsols235 (nsols (* 2 3 5))
	base (* 2 2 2 2 3 5 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 54 (* 45 %)) [0 1 2 3 4])
	   [54 99 144 189 234])))

  (let [nsols235 (nsols (* 2 3 5))
	base (* 2 2 2 2 3 5 5 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 81 (* 63 %)) [0 1 2 3 4])
	   [81 144 207 270 333])))
  ;; .. here
  (let [nsols235 (nsols (* 2 3 5))
	base (* 2 2 2 2 2 3 5 5 5 5)
	case [base (* base 3) (* base 3 3) (* base 3 3 3) (* base 3 3 3 3)]]
    (is (= (map #(- (nsols %) nsols235) case)
	   (map #(+ 108 (* 81 %)) [0 1 2 3 4])
	   [108 189 270 351 432])))
  )

;;; other approaches..

(deftest ultimate-test
  ;; in the forums for problem 108, someone is solving the one-billion problem
  ;; and is giving a few results leading up to the goal.
  ;; basically, factors ar tested and tried, until a minimum is found.

  ;; to test the formular-producer and f-p functions, we let loose on these problems. 
  
  ;; problem110> (factors 2768774904222066200260800)
  ;; [2 2 2 2 2 2 3 3 3 5 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59]
  ;; 1 1 1 1 1 1 1 71 5,57940830126699E26 1743392201 
  ;; 2 2 1 1 1 1 1 67 4,71499293064816E25 1614252038 
  ;; 3 1 1 1 1 1 1 67 3,14332862043211E25 1355971712 
  ;; 2 2 2 2 1 1 1 61 2,46305600854755E25 1494677813 -- 4 factors
  ;; 3 2 2 1 1 1 1 61 7,03730288156442E24 1255529363 
  ;; 3 3 1 1 1 1 1 61 4,22238172893865E24 1054644665 
  ;; 4 3 3 1 1 1 1 59 3,46096863027758E24 1054644665 
  ;; 5 2 2 2 1 1 1 59 3,23023738825908E24 1096097063 -- 4 factors
  ;; 6 3 2 1 1 1 1 59 2,76877490422207E24 1088125448 

  (is (= 1743392201 (sols 20))) ; ok
  (is (= 1614252038 (+ (sols 19) (* (expt 3 (- 19 3)) ((nth (f-p 1) 1) 1)))))   ; ok 2 2 1 1 1 1 1
  (is (= 1355971712 (+ (sols 19) (* (expt 3 (- 19 3)) ((nth (f-p 3) 0) 0)))))   ; ok 3 1 1 1 1 1 1
  ;; 4 changing factors not supported by f-p, fallback to formular-producer
  ;; (def ff2222 (formular-producer (* 2 3 5 7) (* 2 2 3 3 5 7) 5 7))
  (let [ff2222 (formular-producer (* 2 3 5 7) (* 2 2 3 3 5 7) 5 7)]
    (is (= 1494677813 (+ (sols 18) (* (expt 3 (- 18 4)) ((nth ff2222 1) 1)))))) ; ok 2 2 2 2 1 1 1
  (is (= 1255529363 (+ (sols 18) (* (expt 3 (- 18 3)) ((nth (f-p 3) 1) 1)))))   ; ok 3 2 2 1 1 1 1
  (is (= 1054644665 (+ (sols 18) (* (expt 3 (- 18 3)) ((nth (f-p 3) 2) 0)))))   ; ok 3 3 1 1 1 1 1
  (is (= 1054644665 (+ (sols 17) (* (expt 3 (- 17 3)) ((nth (f-p 4) 2) 2)))))   ; ok 4 3 3 1 1 1 1
  ;; (def ff5222 (formular-producer (* 2 3 5 7) (* 2 2 2 2 2 3 3 5 7) 5 7))
  (let [ff5222 (formular-producer (* 2 3 5 7) (* 2 2 2 2 2 3 3 5 7) 5 7)]
    (is (= 1096097063 (+ (sols 17) (* (expt 3 (- 17 4)) ((nth ff5222 1) 1)))))) ; ok 5 2 2 2 1 1 1
  
  (is (= 1088125448 (+ (sols 17) (* (expt 3 (- 17 3)) ((nth (f-p 6) 2) 1)))))   ; ok 6 3 2 1 1 1 1
  (defn searcher [target k v] (quot (- target (sols k)) (* (expt 3 (- k v)))))
  (searcher 1000000000 17 3)
  ;; 195
  ;;--- 214 is the closest match
  (is (= 214 (quot (- 1088125448 (sols 17)) (expt 3 (- 17 3))))
      (= 214 ((nth (f-p 6) 2) 1)))
  
  ;; if we could figure out which formular-producer to use easily ..
  ;; in the above case, we search for one that returns 214 for 3 repeated primes
  
  
  ;; (not correct, but suggested in the forums)
  ;; 5 4 3 3 2 2 1 23
  ;;(is (+ (sols 9)) (* (expt 3 (- 9 6))) ((nth (f-p 6) 3) 2)) .. missing factors
  
  )

;; exactly the same argument and deductions used in the forums:
;;
;; As (expt 3 15) = 14348907 a sequence of 15 primefactors gives a solution although not a minimal one.
;; (we want to be as close to and above 8 million as possible)
;; So we start with
;; (* 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
;; To reduce n, we now try to replace one of the factors with a smaller one
;; try to replace 47
;; (* 3 3 3 3 3 3 3 3 3 3 3 3 3 5) = 7971615
;; it doesn't fly .. taking one factor out adding two smaller ones only reduces solutions a bit


(deftest problem-108
  (is (= (nsols 180180)
	 (+ (sols 6)
	    ;; number of solusions for mimimum number with 6 distinct primes
	    ;; 2 3 5 7 11 13
	    (* (expt 3 4)
	       ((nth formular1 1) 1)))
	 ;; 3^(k-p), k= #of distinct primes, p #number of repeated primes
	 ;; fa^2, fb^2, forumlar1 returns the difference fuction for 2 repeated primes
	 1013))
  (is (= (factors 180180)
	 [2 2 3 3 5 7 11 13])))

(deftest formular1
  ;; 6 distinct primes, one of those repeated. formular1 can be used in 2 different ways
  (is (= (nsols (* 2 2 3 5 7 11 13)) (nsols (* 2 3 3 5  7 11 13)) (nsols (* 2 3 5 5  7 11 13))
	 (nsols (* 2 3 5 7 7 11 13)) (nsols (* 2 3 5 7 11 11 13)) (nsols (* 2 3 5 7 11 13 13))
	 (+ (sols 6) (* (expt 3 4) ((nth formular1 0) 1)))
	 (+ (sols 6) (* (expt 3 4) ((nth formular1 1) 0)))
	 608)))

;; ((nth (f-p m) n) j)   == (* p1^m p2^(n+1) p3^(j+1))
;; ((nth (formular-producer (* 2 3 5) (* (reduce * (repeat m 2)) 3 5) 3 5) n) j)
;; ((nth (formular-builder (nth (variable-factors-3) m)) n) j) = (* p1^(m+1) p2^(n+1) p3^(j+1))

;; 3 factors
;;[[0 9 9 6] [9 15 15 10] [18 21 21 14] [27 27 27 18] [36 33 33 22] [45 39 39 26] [54 45 42 30]]

(defn variable-factors-3 []
  (iterate (fn [[a b c d]] [(+ a 9) (+ b 6) (+ c 6) (+ d 4)]) [0 9 9 6]))
;;(frequencies base) can tell how many repeated primes that are present and how many primes there are
;; (= k (count (frequencies base)) (count (frequencies sample)))
;;(count (filter #(< 1 %) (vals (frequencies (factors (* 2 2 3 3 5))))))

(comment
  ;;NEED
  (formular-producer [2 1 1]) ;; simply give the powers of the factors
  (formular-producer [5 4 3 2 1]) ;; 5 factors
  )
;;(defn formular-producer [sample base p1 p2]
;;  (formular-builder (formular-calculator sample base p1 p2)))

(deftest variable-factors-3
  (is (= ((nth (f-p 3) 1) 1))
      ((nth (formular-builder (nth (variable-factors-3) 2)) 1) 1)))

(defn variable-factors-4 []
  (iterate (fn [[a b c d]] [(+ a 9) (+ b 6) (+ c 6) (+ d 4)]) [0 9 9 6]))

;; (* p1^3 p2^m p3^(n+1) p4^(j+1))
;; [54 63 63 42] [117 105 05 70] [180 147 147 98] [243 189 189 126] [306 231 231 154]]

(comment
  ;; ((nth (formular-producer (* 2 3 5) (* (reduce * (repeat m 2)) 3 5) 3 5) n) j)
  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7) 3 5))
  {:results ((54 117) (117 222)), :a 54, :b 63, :c 63, :d 42}

  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7 7) 3 5))
  {:results ((117 222) (222 397)), :a 117, :b 105, :c 105, :d 70}

  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7 7 7) 3 5))
  {:results ((180 327) (327 572)), :a 180, :b 147, :c 147, :d 98}

  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7 7 7 7) 3 5))
  {:results ((243 432) (432 747)), :a 243, :b 189, :c 189, :d 126}

  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7 7 7 7 7) 3 5))
  {:results ((306 537) (537 922)), :a 306, :b 231, :c 231, :d 154}

  )

(comment
  ;; ((nth (formular-producer (* 2 3 5) (* (reduce * (repeat m 2)) 3 5) 3 5) n) j)
  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7) 3 5))
  {:results ((54 117) (117 222)), :a 54, :b 63, :c 63, :d 42}

  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7 7) 3 5))
  {:results ((117 222) (222 397)), :a 117, :b 105, :c 105, :d 70}

  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7 7 7) 3 5))
  {:results ((180 327) (327 572)), :a 180, :b 147, :c 147, :d 98}

  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7 7 7 7) 3 5))
  {:results ((243 432) (432 747)), :a 243, :b 189, :c 189, :d 126}

  (def f7 (formular-producer (* 2 3 5 7) (* 2 2 2 3 5 7 7 7 7 7) 3 5))
  {:results ((306 537) (537 922)), :a 306, :b 231, :c 231, :d 154}

  )

;; ----------------------------------------------------------------------------
;; al the above calculations, will eventually lead to the formular:
;; n = (p1^a1)(p2^a2)...(pt^at),
;; a(n) = ((2 a1 + 1)(2 a2 + 1) ... (2 at + 1) + 1)/2
;; The formular is listed here:
;;   http://www.research.att.com/~njas/sequences/A018892
;; Unfortunately, brute force is out of the question, so we need to use another
;; sequence:
;;   problem108.filter-smaller-than-seen
;; more commonly known as:
;;   http://www.research.att.com/~njas/sequences/A126098

(defn xsols [n]
  (quot  (+ 1 (reduce * (map #(+ (* 2 (val %)) 1) (frequencies (factors n))))) 2))

(deftest test-xsols
  (is (= 1743392201 (xsols (reduce * (take 20 primes)))))                       ; ok 1 1 1 1 1 1 1 71
  (is (= 1614252038 (xsols (* 2 3  (reduce * (take 19 primes))))))              ; ok 2 2 1 1 1 1 1 71
  (is (= 1355971712 (xsols (* 2 2 (reduce * (take 19 primes))))))               ; ok 3 1 1 1 1 1 1 67
  (is (= 1494677813 (xsols (* 2 3 (reduce * (take 19 primes))))))               ; ok 2 2 2 2 1 1 1 67
  (is (= 1255529363 (xsols (* 2 2 3 5 (reduce * (take 18 primes))))))           ; ok 3 2 2 1 1 1 1 61
  (is (= 1054644665 (xsols (* 2 2 3 3 (reduce * (take 18 primes))))))           ; ok 3 3 1 1 1 1 1 61
  (is (= 1054644665 (xsols (* 2 2 2 3 3 5 5 (reduce * (take 17 primes))))))     ; ok 4 3 3 1 1 1 1 59
  (is (= 1096097063 (xsols (* 2 2 2 2 3 5 7 (reduce * (take 17 primes))))))     ; ok 5 2 2 2 1 1 1 59
  (is (= 1088125448 (xsols (* 2 2 2 2 2 3 3 5 (reduce * (take 17 primes))))))   ; ok 6 3 2 1 1 1 1 59
  )

;; from the start, we knew that the result would be found in this sequence, so
;; we will try to produce this sequence. (for very much higher numbers)

(def a126098
     [1 2 4 6 12 24 30 60 120 180 210 360 420 840 1260 1680 2520 4620 7560 9240
      13860 18480 27720 55440 83160 110880 120120 180180 240240 360360 720720
      1081080 1441440 1801800 2042040 2882880 3063060 4084080 5405400 6126120
      12252240 18378360 24504480])

(def a126098-factors
     (map #(factors %) a126098))

(defn max-powers
  "max powers return the maximum power of each of the primes forming the
  current step. Not all the combinations of powers produce an item in
  a126098, but all the numbers in the sequence will be produced.
  example:
  (4 2 1 1) for 4 factor numbers means that 2 and 3 will have the maximum
  powers of 4 and 2, but not neccesarily in the same set of factors.
  Both (* 2 2 2 2 3 3 5 7) and (* 2 2 3 5 7 11) will have 203 solusions,
  with the latter being smaller (and hence the first to make a new record)"
  [k]
  (take k (map #(quot k %)  (iterate inc 1))))

;; TODO: a function that will return (reduce * factors) for each combination
;; of powers returned by max powers.

;; (2 1)
(comment
  (for [p1 (range 1 4) p2 (range 1 3) n (* (expt 2 p1) (expt 3 p2))]
    {:sols (xsols n) :n n}))

;; calculate all the possible factor combinations for the group
;; sort them by their solutions and 
;; select the smallest combination for each solution
;; calculate the next group and
;; start smerging the results together
;; at about 11 factors, the answer for problem 110 will be produced

;; we can use smerge from the Hamming number problem (204)
;; to merge two "factor blocks"
(use '[problem204 :only (smerge)])

(comment
  (map  #(max-powers %) (range 1 8))
  ;;((1) (2 1) (3 1 1) (4 2 1 1) (5 2 1 1 1) (6 3 2 1 1 1) (7 3 2 1 1 1 1))
  (map #(vals %) (map #(frequencies %) a126098-factors))
  ;;((2) (3 1) (3 2 1) (4 3 1 1) (5 3 1 1 1) (6 3 2 1 1 1))
  ;; close enough for rock'n'roll
  )

;; as things are not matching perfectly and as we know that we do not have to
;; go over a very limited number of different primes (the one billion problem
;; only uses 17 unique primes, which gives us a nice limited upper bound)

;; by looking at the factorisation in a126098-factors something like max-powers
;; has been produced, but it's not entirely perfect for making a126098 a seqence.

;; but good enough for rock'n'roll ..

(defn records
  [k]
  (let [ps k
	s (count (filter #(< 1 %) (max-powers ps)))
	[pn pk] (split-at s (take ps primes))
	pkn (reduce * pk)]
    (sort (into #{}
		(for [p1 [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]
		      p2 [1 2 3 4 5 6 7 8]
		      p3 [1 2 3 4 5]
		      p4 [1 2 3 4]
		      p5 [1 2 3 4]
		      p6 [1 2 2 4]
		      p7 [1 2 2 4]]
		  ;; too many combinations are produced
		  (* (reduce * (map (fn [base pow] (expt base pow)) pn
				    (take s [p1 p2 p3 p3 p4 p5 p6 p7]))) pkn))))))

(comment
  ;; the result closest to 4000000 observed is this
  (filter #(= 4018613 (:sols %)) (map #(hash-map :sols (xsols %) :n %)  (records 11))))

(comment
  (filter #(< 4000000 (xsols (:n %)) 4020000) (map #(hash-map :sols (xsols %) :n %)  (records 11)))
  (filter #(< 4000000 (xsols (:n %)) 4020000) (map #(hash-map :sols (xsols %) :n %)  (records 10))))

(defn problem110 []
  (first (sort
	  [(first (map #(min (:n %)) (filter #(< 4000000 (xsols (:n %)) 4100000) (map #(hash-map :sols (xsols %) :n %)  (records 10)))))
	   (first (map #(min (:n %)) (filter #(< 4000000 (xsols (:n %)) 4100000) (map #(hash-map :sols (xsols %) :n %)  (records 11)))))
	   (first (map #(min (:n %)) (filter #(< 4000000 (xsols (:n %)) 4100000) (map #(hash-map :sols (xsols %) :n %)  (records 12)))))])))


(defn find-lowest-one [factors]
  (first (map #(min (:n %)) (filter #(< 4000000 (xsols (:n %)) 4100000)
				    (map #(hash-map :sols (xsols %) :n %)  (records factors))))))
(defn problem110 []
  (first (sort (map #(find-lowest-one %) [10 11 12]))))

;; problem110> (time (problem110))
;; "Elapsed time: 13919.281935 msecs"
;; 9350130049860600
