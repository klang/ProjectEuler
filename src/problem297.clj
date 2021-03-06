(ns problem297
  (meta {:description "Each new term in the Fibonacci sequence is generated by adding the previous two terms.
Starting with 1 and 2, the first 10 terms will be: 1, 2, 3, 5, 8, 13, 21, 34, 55, 89.

Every positive integer can be uniquely written as a sum of nonconsecutive terms of the Fibonacci sequence. For example, 100 = 3 + 8 + 89.
Such a sum is called the Zeckendorf representation of the number.

For any integer n>0, let z(n) be the number of terms in the Zeckendorf representation of n.
Thus, z(5) = 1, z(14) = 2, z(100) = 3 etc.
Also, for 0<n<10^6, ∑ z(n) = 7894453.

Find ∑ z(n) for 0<n<10^17."
	 :observations "F83 < 10^17 < F84"})
  (:use
   [clojure.test :only (deftest is)]
   [tools.misc :only (indexed)]
   [tools.numbers :only (fibos)]
   [clojure.math.numeric-tower :only (expt) :as math]))

(defn flatten-once [s] (remove seq? (tree-seq seq? seq s)))
(def hard-fibs (into [] (take-while #(< %(expt 10 17)) (drop 2 (fibos)))))
(def hard-fibs-reverse (into [] (reverse (take-while #(< %(expt 10 17)) (drop 2 (fibos))))))

(defn zeckendorf-hard [number]
  "returns the zeckendorf representation of a number, based on a hardcoded list"
  (loop [number number catch (transient [])]
    (if (zero? number) (persistent! catch)
      (let [n (first (drop-while #(< number %) hard-fibs-reverse))]
	(recur (- number n) (conj! catch n))))))

(defn zeckendorf-terms-hard [number]
  "returns the number of terms in the zeckendorf representation of a number, based on a hardcoded list"
  (loop [number number terms 0]
    (if (zero? number) terms
      (let [n (first (drop-while #(< number %) hard-fibs-reverse))]
	(recur (- number n) (inc terms))))))

(deftest test-zeckendorf-hard 
  (is (= [89 8 3] (zeckendorf-hard 100)))
  (is (= 3 (zeckendorf-terms-hard 100))))

(defn zeckendorf [number]
  "returns the zeckendorf representation of a number"
  (loop [number number catch (transient [])]
    (if (zero? number) (persistent! catch)
      (let [n (last (take-while #(<= % number) (fibos)))]
	(recur (- number n) (conj! catch n))))))

(defn zeckendorf-terms [number]
  "returns the number of terms in the zeckendorf representation of a number"
  (loop [number number terms 0]
    (if (zero? number) terms
      (let [n (last (take-while #(<= % number) (fibos)))]
	(recur (- number n) (inc terms))))))

(deftest test-zeckendorf 
  (is (= 1 (zeckendorf-terms 5)))
  (is (= 2 (zeckendorf-terms 14)))
  (is (= 3 (zeckendorf-terms 100)))
  (is (= [89 8 3] (zeckendorf 100)))
  (is (= 3 (zeckendorf-terms 100))))

(defn F [n]
  "Stack consuming fibo, Programming Clojure, p.133"
  (cond 
    (<= n 0) 0
    (= n 1) 1
    :else
    (+ (F (- n 1)) (F (- n 2)))))

(defn Zr [n]
  "Stack consuming Sum of Zeckendorf-terms" 
  (cond
    (= n 0) 1
    (= n 1) 1
    :else
    (+ (Zr (- n 1)) (Zr (- n 2)) (F (- n 1)))))

(defn Z []
  "returns the sum of the number of terms in fibonacci points"
  (map first (iterate (fn [[z2 z1 f2 f1 ]] [z1 (+ z2 z1 f1) f1 (+ f2 f1)]) [1 1 0 1])))

(deftest test-Zzz
  (is (= (map #(Zr %) (range 0 14))
	 '(1 1 3 5 10 18 33 59 105 185 324 564 977 1685)
	 (take 14 (Z)))))

(defn find-index [fib-num]
  "find the index of a given fibonacci number" 
  (first (first (filter #(= (second %) fib-num) (indexed hard-fibs)))))

(defn group [nth-fib]
  "returns the length of the zeckendorf terms for the group leading up to the nth fibonacci number.
Counting from zero in [1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181 6765 10946 .. ]"
  (take (- (dec (nth hard-fibs nth-fib)) 
	   (dec (nth hard-fibs (dec nth-fib))))
	(map #(zeckendorf-terms-hard %) (iterate inc (nth hard-fibs (dec nth-fib))))))

(defn partial-group [nth-fib]
  (take (- (nth (fibos) nth-fib) (nth (fibos) (dec nth-fib)))
	(map #(zeckendorf-terms-hard %) (iterate inc (+ (nth (fibos) (inc nth-fib))
							(nth (fibos) (dec nth-fib)))))))



(deftest test-zeckendorf-sums
  (is (= (reduce + (take 987 (map #(zeckendorf-terms-hard %) (iterate inc 1))))
	 3971)))

  ;; when the target is a fibonacci number 
  (is (= (reduce + (take (dec 987) (map #(zeckendorf-terms-hard %) (iterate inc 1))))
	 (reduce + [1 1 3 5 10 18 33 59 105 185 324 564 977 1685])
	 3970
	 (reduce + (take 14 (Z)))
	 (reduce + (take (find-index 987) (Z)))))

  ;; when the target is not a fibonacci number
  (is (= (nth (Z) (inc (find-index  (- 1000 (last (take-while #(<= % 1000) (fibos)))))))
	 33
	 ;; (zeckendorf-hard 33) => [21 8 3 1]
	 (+ 1 2 2 2 3 2 3 3 2 3 3 3 4)))
  ;; 
  (is (= (reduce + (take (dec 1000) (map #(zeckendorf-terms-hard %) (iterate inc 1))))
	 4003
	 (+ 3970 33)
	 ;; (zeckendorf-hard 1000) => [987 13]
	 (+ (reduce + (take (find-index 987) (Z)))
	    (nth (Z) (inc (find-index 13))))))
  ;;
  (is (= (reduce + (take (dec 1025) (map #(zeckendorf-terms-hard %) (iterate inc 1))))
	 4086
	 (+ 3970 105 11)
	 ;; (zeckendorf-hard 1025) => [987 34 3 1]
	 (+ (reduce + (take (find-index 987) (Z)))
	    (nth (Z) (inc (find-index 34)))
	    11 
	    ;; some calculation based on 3
	    ;; some calculation based on 1
	    )))

  (is (= (reduce + (take 100 (map #(zeckendorf-terms-hard %) (iterate inc 1))))
	 (+ (+ 1                                                                      ;1
	       1                                                                      ;2  
	       1 2                                                                    ;3 
	       1 2 2                                                                  ;5
	       1 2 2 2 3                                                              ;8
	       1 2 2 2 3 2 3 3                                                        ;13 
	       1 2 2 2 3 2 3 3 2 3 3 3 4                                              ;21
	       1 2 2 2 3 2 3 3 2 3 3 3 4 2 3 3 3 4 3 4 4                              ;34 
	       1 2 2 2 3 2 3 3 2 3 3 3 4 2 3 3 3 4 3 4 4 2 3 3 3 4 3 4 4 3 4 4 4 5)   ;55
	    ;; only 88 terms in the sum above
	    ;; the term for 89 is the first term (1) in the next group
	    ;; 
	    (+ 1 2 2 2 3 2 3 3                                                        
	       2 3 3 3))                                                              ;89
	 ;; --- for that reason, there is one term too much in the last one
	 (+ 235 18 11)
	 264
	 ;; (zeckendorf-hard 100) => [89 8 3]
	 (+ (reduce + (take (find-index 89) (Z)))
	    (nth (Z) (inc (find-index 8)))
	    ;; ---> the group that starts from the 9th fibo
	    ;; some calculation based on 3 that adds up to 11
	    ;; ok, why add one?
	    (reduce + (take (+ 3 1) (partial-group (inc (find-index 89))))))))


  ;;
  (comment
    (is (= (reduce + (take 32 (map #(zeckendorf-terms-hard %) (iterate inc 1))))
	   67
	   ;; (zeckendorf-hard 32) => [21 8 3]
	   (+ (reduce + (take (find-index 21) (Z)))
	      (nth (Z) (inc (find-index 8)))
	      (+ (nth (Z) (inc (find-index 3))) (inc (find-index 3)) 
		 3)
	      #_(reduce + (take (+ 3 1) (partial-group (inc (find-index 21))))))))

    (is (= (reduce + (take 10000 (map #(zeckendorf-terms-hard %) (iterate inc 1))))
	   52816
	   ;; (zeckendorf-hard 10000) => [6765 2584 610 34 5 2]
	   (+ 34690 14406 3720)
	   (+ (reduce + (take (find-index 6765) (Z)))
	      (nth (Z) (inc (find-index 2584)))
	      (+ (+ (nth (Z) (inc (find-index 610))) 610)
		 (+ (nth (Z) (inc (find-index 34))) (* 2 34))
		 (+ (nth (Z) (inc (find-index 5))) (* 3 5))
		 (+ (nth (Z) (inc (find-index 2))) (* 4 2)))
	      6 ;; <---- (zeckendorf-terms 10000)
	      ;;(- 3720 2909) 811
	      #_(reduce + (take (+ 610 34 5 2 1) (partial-group (inc (find-index 6765))))))))

    ;; --- something is wrong with the way we calulate the initial terms
    ;; (reduce + (take (find-index 6765) (Z))) only contains the sum of 6764 terms
    ;; (nth (Z) (inc (find-index 2584)))       contains the sum of 2584 terms
    ;; there should only be (+ 610 34 5 2) terms left, but there is one more term

    (is (= 7894453
	   ;;(zeckendorf-hard (expt 10 6)) => [832040 121393 46368 144 55]
	   (+ (reduce + (take (find-index 832040) (Z)))
	      (nth (Z) (inc (find-index 121393)))
	      (+ (+ (nth (Z) (inc (find-index 46368))) 46368)
		 (+ (nth (Z) (inc (find-index 144))) (* 2 144))
		 (+ (nth (Z) (inc (find-index 55))) (* 3 55)))
	      ;;(reduce + (take (+ 46368 144 55) (partial-group (inc (find-index 832040))))))
	      ;;382970
	   ))))

(defn zelastpart [coll]
  (loop [c coll i 1 sum 0]
    (if (empty? c)
      sum
      (recur (rest c) (inc i) (+ sum (+ (nth (Z) (inc (find-index (first c)))) (* i (first c))))))))

(defn Zef [number]
  (let [z (zeckendorf-hard number)
	r (subvec (zeckendorf-hard number) 2)]
    (+ (reduce + (take (find-index (first z)) (Z)))
       (nth (Z) (inc (find-index (second z))))
       (zelastpart r))))

(deftest test-zef
  (is (= 7894453 (Zef 1000000))))

(defn Zef2 [number]
  (let [z (zeckendorf-hard number)]
    (+ (reduce + (take (find-index (first z)) (Z)))
       (zelastpart (rest z)))))

(defn find-index [fib-num]
  "find the index of a given fibonacci number" 
  (- (first (first (filter #(= (second %) fib-num) (indexed (fibos))))) 2))

(defn Zef2 [number]
  "returns ∑ z(n) < number"
  (let [z (zeckendorf number)]
    (+ (reduce + (take (find-index (first z)) (Z)))
       (loop [c (rest z) i 0 sum 0]
	 (if (empty? c)
	   sum
	   (recur (rest c) 
		  (inc i) 
		  (+ sum (nth (Z) (inc (find-index (first c)))) (* i (first c)) )))))))


(deftest test-zef2  
  (is (= 7894453 (Zef2 1000000))))

(defn problem297 [] (Zef2 1000000))
