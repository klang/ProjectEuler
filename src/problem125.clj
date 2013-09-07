(ns problem125
  (meta {:description "The palindromic number 595 is interesting because it can be written as the sum of consecutive squares: 6² + 7² + 8² + 9² + 10² + 11² + 12².

There are exactly eleven palindromes below one-thousand that can be written as consecutive square sums, and the sum of these palindromes is 4164. Note that 1 = 0² + 1² has not been included as this problem is concerned with the squares of positive integers.

Find the sum of all the numbers less than 10⁸ that are both palindromic and can be written as the sum of consecutive squares."})
  (:use
   [clojure.math.numeric-tower :only (expt)]
   [problem004 :only (palindrome?)]
   [clojure.test :only (deftest is)]))

;; 1² 2²  3²  4²  5²  6²  7²
;; 1  4   9  16  25  36  49  64  81 100 121 144 169  196  225
;; 1  5  14  30  55  91 140 204 285 385 506 650 819 1015 1240 
;; 1² 1²+2² 1²+2²+3² 1²+2²+3²+4² 1²+2²+3²+4²+5² 1²+2²+3²+4²+5²+6² 1²+2²+3²+4²+5²+6²+7² ...
;;             2²+3²    2²+3²+4²    2²+3²+4²+5²    2²+3²+4²+5²+6²    2²+3²+4²+5²+6²+7² ...
;;                         3²+4²       3²+4²+5²       3²+4²+5²+6²       3²+4²+5²+6²+7² ...
;;                                        4²+5²          4²+5²+6²          4²+5²+6²+7² ...
;;                                                          5²+6²             5²+6²+7² ...
;;                                                                               6²+7² ...

(defn squared-sums []
  (drop 1 (map first (iterate (fn [[a b]] [(+ a (* b b)) (inc b)]) [0 1]))))

(defn squared-sum-palindromes [n]
  (filter #(< 0 %) 
	  (filter palindrome? 
		  (take-while #(< % 1000) (map #(- % n) (squared-sums))))))

(map #(squared-sum-palindromes %) (take-while #(< % 1000) (squared-sums)))
;; 1 2  3  4  5 6    7   8   9  10  11  12  13   14   15
;; 1 5 14 30 55 91 140 204 285 385 506 650 819 1015 1240 
;; 1           5   14       30 55
;; 1           2   3        4  5     6  7  8     9  10        11
;;((4 505 818) (9) (77 636) () (595) () () (181) () (121 434) (313) () ())
;; 

(defn squared-sum-palindromes [n]
  (filter #(< 0 %) 
	  (filter palindrome? 
		  (take-while #(< % 1000) (map #(- % n) (squared-sums))))))

(defn squared-sums [starting]
  (drop 2 (map first (iterate (fn [[a b]] [(+ a (* b b)) (inc b)]) [0 starting]))))

(defn p125t []
  (reduce + (filter palindrome? 
		    (flatten (map 
			      (fn [x] (take-while #(< % 1000) (squared-sums x)))
			      (range 1 22))))))
(deftest test-p125t
  (is (= 4164 (p125t))))

;; problem125> (time (p125t))
;; "Elapsed time: 219.868231 msecs"
;; 4164

;; manual binary search for the last upper limit of the range
;; (count (flatten (map (fn [x] (take-while #(< % (expt 10 8)) (squared-sums x))) [7070])))
;; 1
;; (count (flatten (map (fn [x] (take-while #(< % (expt 10 8)) (squared-sums x))) [7071])))
;; 0

(defn p125 []
  (reduce + (filter palindrome? 
		    (flatten (map 
			      (fn [x] (take-while #(< % (expt 10 8)) (squared-sums x)))
			      (range 1 7071))))))

;; problem125> (time (p125))
;; "Elapsed time: 7831.973044 msecs"
;; 2916867073
;; not correct

;; some numbers are counted more than once, as the representation is not unique
(defn p125 []
  (reduce + (into #{}
		  (filter palindrome? 
			  (flatten (map 
				    (fn [x] (take-while #(< % (expt 10 8)) (squared-sums x)))
				    (range 1 7071)))))))

;; problem125> (time (p125))
;; "Elapsed time: 7602.500803 msecs"
;; 2906969179

(defn p125b [limit]
  (loop [x 1 catch #{}]
    (let [ss (take-while #(< % limit) (squared-sums x))]
      (if (empty? ss)
	(reduce + catch)
	(recur (inc x) (into catch (filter palindrome? ss)))))))

;; problem125> (time (p125b (expt 10 3)))
;; "Elapsed time: 1.512762 msecs"
;; 4164
;; problem125> (time (p125b (expt 10 8)))
;; "Elapsed time: 4049.981541 msecs"
;; 2906969179
(defn problem125 [] (p125b (expt 10 8)))

