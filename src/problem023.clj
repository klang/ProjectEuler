(ns problem023
  (:use	[tools.primes :only (primes divisors prime?)]
	[clojure.math.numeric-tower :only (expt exact-integer-sqrt)]
	[clojure.test :only (deftest is)])
  (:require
   	[clojure.math.combinatorics :only (selections) :as comb]))

(defn sum-of-proper-divisors [n] (- (reduce + (divisors n)) n))

(defn deficient [n] (< ( sum-of-proper-divisors n) n))
(defn abundant [n] (> ( sum-of-proper-divisors n) n))
(defn perfect [n] (= ( sum-of-proper-divisors n) n))

(def limit 28123)
(def abundant-numbers (filter abundant (iterate inc 1)))

;; lazy sequence, that gives the sums of abundant numbers
(comment (def asums (map #(+ (nth % 0) (nth % 1)) (comb/selections abundant-numbers 2))))
;; problem is, that there are a lot (* 6965 6965) 48511225 which is way
;; more combinations than there are numbers under the limit given

;; creating the full list of numbers under the limit, that are asums
;; should be possible .. but takes a while
(comment (def asums-set (into #{} (filter #(<= % limit) asums))))

;; a lot of the combinations are not necesary, maybe we can keep under the limit as we go?
(def asums (map #(+ (nth % 0) (nth % 1)) 
		(filter #(<= (+ (nth % 0) (nth % 1)) limit)
			(comb/selections abundant-numbers 2))))
;; should only give the the numbers under the limit that are sums of two abundant numbers
;;(def asums-set (into #{} (filter #(<= % limit) asums)))
;; this also takes a while, and runs out of memory

;;;-----------------------------
(def anumbers (take-while #(< % limit) abundant-numbers))
;; problem023> (time (count anumbers))
;; "Elapsed time: 18222.815612 msecs"
;; 6965

;;; ---------------------------- let's use a new trick! -----------------------

;; the efficient version of problem 179, modified to return the sum of divisors
(defn make-divs-seq [limit]
  "an int-array containing the sum of divisors for index"
  (let [limit (inc limit)] 
    (loop [i (int 0), d (int 2), divs (int-array limit 1)]
      (if (= d limit) 
	divs
	(if (>= i limit) 
	  (recur (int 0) (inc d) divs)
	  (recur (+ i d) d (do (aset divs i (+ (aget divs i) d)) divs)))))))

(deftest test-make-divs-seq
  ;; "Elapsed time: 23760.152414 msecs"
  (let [s (make-divs-seq limit)]
    (is (= 28123 limit))
    (is (prime? limit))
    (is (= 28124 (nth s limit) (last s)))
    (is (= 395465626 (first s) (reduce + (range 1 (+ 1 limit)))))))

(defn abundants [divs]
  (let [indexes (iterate inc 0)]
    (filter #(< 0 %) (map #(if (> (- %2 %1) %1) %1 0) indexes divs))))

;; for i>1 it holds that a(i+1) = ai + a
;; if the sum of the divisors of a is higher than a (i.e. a is abundant), 
;; then a(i+1) is also abundant, as a is one of the divisors of a(i+1)

;; we will make a set of "reduced-abundant-numbers", i.e. the first abundant
;; number in an a*i chain 

;; take the first element, 
;; put the first element in 'reduced
;; recur with a, where all multiples of the first element is removed
;; will give a list reduced list .. 12,18,20,30... 
(defn reduced-anumbers [limit]
  (loop [a (abundants (make-divs-seq limit))
	 reduced []]
    (if (zero? (count a)) ;; why does (empty? a) give a stack overflow?
      reduced
      (recur (filter #(not (zero? (rem % (first a)))) a)
	     (conj reduced (first a))))))

(comment (def ra (reduced-anumbers limit)))
;;problem023> (count (combinations ra 2))
;;472878

;; why reduce at all? .. 

(comment
  (def x (make-divs-seq limit))
  (take 29 (abundants x))
  (take 29 anumbers)
  (def d12 (filter #(not (zero? (rem % (first (abundants x))))) (abundants x)))
  (def d18 (filter #(not (zero? (rem % (first d12)))) d12))
  (def d20 (filter #(not (zero? (rem % (first d18)))) d18))
  (def d30 (filter #(not (zero? (rem % (first d20)))) d20))
  (def d42 (filter #(not (zero? (rem % (first d30)))) d30))
  (filter #(< % limit) (distinct (map (fn [[ x y]] (+ x y)) (combinations [12 18 20 24 30 36 40] 2))))
  (distinct (map (fn [[ x y]] (let [z (+ x y)] (if (< z limit) z 0))) (combinations [12 18 20 24 30 36 40] 2)))
)

;; (range 12 53)
;; (take 9 (abundants (make-divs-seq limit)))
;; the next lines have to be made
;;(12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52)
;;(12                18    20          24                30                36          40    42                48)
;;(                                    24                                  36                                  48)
;;(                                                    12+18  12+20      12+24             12+30                         12+40)
;;(                                                                            18+20       18+24             18+30)

(comment
  (def b (into #{} (take-while #(<= % limit)  (map #(+ (first a) %) a))))
  (def c (into b (take-while #(<= % limit)  (map #(+ (second a) %) (rest a)))))
  (= 266882260 (- 395465626 (reduce + c))) ;; new upper bound for answer
  (def d (into c (take-while #(<= % limit)  (map #(+ (nth a 2) %) (rest (rest a))))))
  (= 220667056 (- 395465626 (reduce + d))) ;; new upper bound for answer
  ;; at some point this number will stabilize or we will run out of numbers in a
)

(defn sums-of-abundant-numbers [limit]
  ;; just subract (first (make-divs-seq limit)) from the sums-of-abundant-numbers to get the answer to problem 23
  (loop [a (abundants (make-divs-seq limit)); we might be using (reduced-anumbers limit) instead
	 sums #{}]
    (if (zero? (count a))
      sums
      (recur (rest a) (into sums (take-while #(<= % limit) (map #(+ (first a) %) a))) ))))

(comment
  (deftest test-sums-of-abundant-numbers
    (is (= 24 (first (sort s))))
    (is (= 28123 (last (sort s)))))
)

;; problem023> (time (def s (sums-of-abundant-numbers limit)))
;; "Elapsed time: 80674.246856 msecs"
;; slightly over the limit

;; problem023> (reduce + (range 1 (+ 1 limit)))
;; 395465626
;; problem023> (reduce + s)
;; 391285755 
;; problem023> (- 395465626 (reduce + s))
;; 4179871


(defn sums-of-abundant-numbers [limit]
  ;; just subract (first (make-divs-seq limit)) from the sums-of-abundant-numbers to get the answer to problem 23
  (let [divs (make-divs-seq limit)
	total (first divs)]
    (loop [a (abundants divs)
	   sums #{}]
      (if (zero? (count a))
	(- total (reduce + sums))
	(recur (rest a) (into sums (take-while #(<= % limit) (map #(+ (first a) %) a))) )))))

;; problem023> (time (sums-of-abundant-numbers limit))
;; "Elapsed time: 61931.67402 msecs"
;; 4179871
(defn problem023 [] (sums-of-abundant-numbers limit))
