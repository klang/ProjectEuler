(ns problem050
  #^{:description "The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13

This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?"
     :question "this is an advanced version of problem 31, right?"
     :answer "restricted partitions, yes"
     :info "(count (take-while #(< % 1000000) primes)) ==> 78498"
     :idea "use the old divisors trick, maybe?"
}
  (:use tools.primes)
  (:use clojure.contrib.lazy-seqs)
  (:use clojure.test))

(deftest test-problem050
  (is (prime? (reduce + (take 21 (drop 3 primes)))) )
  #_(is (= (filter prime? (filter #(< % 1000) (map #(- % (nth psums 2) ) psums)))
	 '(7 31 67 271 491 953))
      )
  #_(is (= 953 (last (filter prime? (filter #(< % 1000) (map #(- % (nth psums 2) ) psums))))))
  )
;; 2 2+3 2+3+5 2+3+5+7 2+3+5+7+11 2+3+5+7+11+13 ...
;;         i-1   i
;;           [i]-[i-1] sum starting from i
;;           

;; add the first element of plimit to every element in sums (none by init)
;; add the second element of plimit to every element from the second
(defn make-prime-sum-seq [plimit limit]
  (loop [i (int 2)			; the current index
	 p (int 2)			; the current prime
	 sums (int-array limit 2)]      ; the sum of the primes up to index
    (if (= p limit) 
      sums
      (if (>= i limit)
	(recur (int 0) (inc p) sums)
	(recur (inc i) p (do (aset sums i (+ (nth plimit p) (aget sums i))) sums))))))

;;(def plimit (take-while #(< % 1000000) primes))
;;(def psums (make-prime-sum-seq plimit 78498))
;; last element of psums equals the sum of those 78498 primes
;; check that this number is within the range of an int. 

;; problem050> (reduce + plimit)
;; 37550402023

;; no

;; problem050> (prime? 37550402023)
;; true

(defn make-prime-sum-seq [plimit]
  (loop [p (rest plimit)
	 sums [(first plimit)]]
    (if (= '() p) 
      sums
      (recur (rest p) (conj sums (+ (sums (-  (count sums) 1)) (first p)))))))

(comment
  (def plimit (take-while #(< % 1000) primes))
  (def psums (make-prime-sum-seq plimit))
  (map (fn [i] [i (last (filter prime? (filter #(< % 1000) (map #(- % (nth psums i) ) psums))))]) (range 1 10))
  (def counts (drop-last
	       (map (fn [i] [i (last (filter prime? (filter #(< % 1000) (map #(- % (nth psums i)) psums))))]) 
		    (range 1 (count psums))))))
(def pnum 1000000)
(def plimit (take-while #(< % pnum) primes))
(def psums (make-prime-sum-seq plimit))
(def counts (drop-last 
	     (map (fn [i] [i (last (filter prime? (filter #(< % pnum) (map #(- % (nth psums i)) psums))))]) 
		  (range 1 (count psums)))))
(def plimiti (zipmap plimit (iterate inc 1)))
(def psumsi (zipmap psums (iterate inc 1)))
  

;; problem050> (time (count plimit-full))
;; "Elapsed time: 20539.155341 msecs"
;; 78498
;; 40 seconds left to solve the problem

(defn find-max-length [plengths]
  (loop [p plengths
	 mp [0 0]]
    (if (= '() p)
      mp
      (if (< (first mp) (first (first p)))
	(recur (rest p) (first p))
	(recur (rest p) mp)))))

(deftest test-find-max-length
  (let [pnum 1000
	plimit (take-while #(< % pnum) primes)
	psums (make-prime-sum-seq plimit)
	counts (drop-last 
	       (map (fn [i] [i (last (filter prime? (filter #(< % pnum) (map #(- % (nth psums i)) psums))))]) 
		    (range 1 (count psums))))
	psumsi (zipmap psums (iterate inc 1))
	plengths (map (fn [[num sum]] [(- (psumsi (+ (nth psums num) sum)) num 1) sum]) (drop-last counts))])
  (is (= [21 953] (find-max-length plengths)))
)

;;problem050> (find-max-length (take 1000 plengths))
;;[543 997651]
(defn problem050 []
  (let [pnum 1000000
	plimit (take-while #(< % pnum) primes)
	psums (make-prime-sum-seq plimit)
	counts (drop-last 
	       (map (fn [i] [i (last (filter prime? (filter #(< % pnum) (map #(- % (nth psums i)) psums))))]) 
		    (range 1 (count psums))))
	psumsi (zipmap psums (iterate inc 1))
	plengths (map (fn [[num sum]] [(- (psumsi (+ (nth psums num) sum)) num 1) sum]) (drop-last counts))]
    (second (find-max-length (take 1000 plengths)))))