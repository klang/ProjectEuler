(ns tools.sieve
  (:use [clojure.test :only (deftest is)]))

(comment
  (defn sieve [xs]
    (filter #(not (zero? (mod % (first xs)))) (rest xs)))

  (defn rsieve [xs]
    (map first (iterate sieve xs)))

  (defn primes-below [max]
    (take-while #(not (nil? %)) 
		(rsieve (range 2 max)))))

;; My last post gave a simple functional definition of Eratosthenes Sieve, taken from the 1988, Introduction to Functional Programming by Bird and Philip. 

;; I failed to provide the full example and what I ended up writing was not complete.

;; --- p175
;; primes = map hd (iterate sieve [2..])
;; sieve (p : xs) = [x|x <- xs; x mod p != 0]

;; where x:xs equals the head of the list concatenated with the rest of the list
;; (= xs (cons (first xs) (rest xs)))
;; here p:xs is used to indicate that the head of the list is prime

(comment
  (defn sieve [xs]
    (filter #(not (zero? (mod % (first xs)))) (rest xs))))

(defn sieve [xs]
  (for [x (rest xs)
	:let [p (first xs)]
	:when (not (zero? (mod x p)))] x))

(comment
  (def primes (map first (iterate sieve (drop 2 (range)))))
  )

;; take 100 primes
;; (take 100 primes)
;; takewhile (< 100) primes
;; (take-while #(< % 100) primes)

;;(count (take-while #(< % 8000) primes))
;; will overflow the stack

;; Now, the book suggests, that the reason is, that we are generating an infinite list of infinite lists, which is the reason why this definition is not as efficient as it could be. It is suggested that the above definition of primes can be rewritten as

;; primes    = rsieve [2..]
;; rsieve xs = map hd (iterate sieve xs)


(defn rsieve [xs]
  (map first (iterate sieve xs)))

(def primes (rsieve (drop 2 (range))))
;; this is not more efficient than the original version, but it can be manipulated more easily



;; rsieve xs = map hd (iterate sieve xs)
;; <=> [instantiating p:xs for xs in the definition for rsieve]
;; rsieve (p:xs) = map hd (iterate sieve (p:xs))
;;               [using the definition of iterate]
;;               = map hd ((p:xs):iterate sieve (sieve (p:xs)))
;;               [using the definition of map and hd]
;;               = p : map hd (iterate sieve (sieve (p:xs)))
;;               [using the definition of sieve]
;;               = p : map hd (iterate sieve [x|x<-xs; x mod p != 0])
;;                     |-------------------|<--- that is just an instance of rsieve
;;               = p : rsieve [x|x<-xs; x mod p != 0]
;; in short
;; rsieve (p:xs) = p : rsieve [x|x<-xs; x mod p != 0]

;; this definition uses recursion explicitly while e the first did not. Furthermore
;; the new definition does not involve an infinite number of infinite lists


(defn rsieve [xs]
  (cons (first xs)
	(for [x (rest xs)
	      :let [p (first xs)]
	      :when (not (zero? (mod x p)))] x)))

(defn rsieve [xs]
  (lazy-seq
   (cons (first xs)
	 (for [x (rest xs)
	       :let [p (first xs)]
	       :when (not (zero? (mod x p)))] x))))

(defn rsieve [xs]
  (lazy-seq
   (cons (first xs)
	 (filter #(not (zero? (mod % (first xs)))) (rest xs)))))

(def primes (rsieve (drop 2 (range))))

(deftest test-primes
  (is (= 168 (count (take-while #(< % 1000) primes))))
  (is (= 669 (count (take-while #(< % 5000) primes))))
  (is (= 900 (count (take-while #(< % 7000) primes))))
  (is (= 977 (count (take-while #(< % 7703) primes))))  
  (is (= 1007 (count (take-while #(< % 8000) primes)))) ;;<<--- stack overflow
 )
#_(run-tests)

