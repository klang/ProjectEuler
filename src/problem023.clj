(load "tools")

;; naive way to find number of divisors
(use 'clojure.contrib.combinatorics)
(use 'clojure.set)

(defn divisors [n] (set (map #(reduce * %) (subsets (factors n)))))
(defn proper-divisors [n] (disj (divisors n) n))
(defn sum-of-proper-divisors [n] (reduce + (proper-divisors n)))
; using a cheaper operation
(defn sum-of-proper-divisors [n] (- (reduce + (divisors n)) n))

(defn deficient [n] (< ( sum-of-proper-divisors n) n))
(defn abundant [n] (> ( sum-of-proper-divisors n) n))
(defn perfect [n] (= ( sum-of-proper-divisors n) n))

(def limit (+ 28123 1))
(def abundant-numbers (filter abundant (iterate inc 1)))
;; user> (count (take-while #(< % limit) abundant-numbers))
;; 6965
;;(map #(+ (nth % 0) (nth % 1)) (selections (proper-divisors 24) 2))
;;(map #(+ (nth % 0) (nth % 1)) (selections (proper-divisors 30) 2))

;; selections is lazy. hopefully, reverse can take the selections
;; from the end, without having to realize the full sequence.
(defn sum-of-two-abundant-numbers [n] 
      (not (not-any? #(= n (+ (nth % 0) (nth % 1))) 
		     (reverse (selections (proper-divisors n) 2)))))

(defn p [n] 
  (not-any? #(= n (+ (nth % 0) (nth % 1))) 
	    (reverse (selections (proper-divisors n) 2))))

;; (reduce + (filter #(p %) (range 1 limit)))
;; user> (time (reduce + (filter #(p %) (range 1 limit))))
;; "Elapsed time: 56687.514128 msecs"
;; 197739844
;; -- not really efficient
;; .. not really the right answer, either.
;; user> (time (reduce + (filter #(p %) (range 1 28123))))
;; "Elapsed time: 57687.409948 msecs"
;; 197711721
;; .. damn, not it..

(defn p [n] 
  (not-any? #(= n (+ (nth % 0) (nth % 1))) 
	       (selections (take-while #(< % n) abundant-numbers) 2)))

;;(time (reduce + (filter #(p %) (range 1 28123))))
;; -- takes too long

;; user> (filter #(abundant %) (take-while #(< % limit) primes))
;; ()
;; all primes are deficient (proper divisors are 1 and p)
;;
;; (reduce + (take-while #(< % limit) primes))
;; 1 is a positive number that fulfills the property

;;user> (time (reduce + (filter #(and (not (prime? %)) p %) (range 2 limit))))
;;"Elapsed time: 1633.678238 msecs"
;;354901679
;; + 1
;; + 40563946

;(time (+ 1 (reduce + (take-while #(< % limit) primes)) (reduce + (filter #(and (not (prime? %)) p %) (range 2 limit)))))
;; "Elapsed time: 1610.757708 msecs"
;; 395465626
;; .. nope!

;; are all primes really expressible as the sum of two abundant numbers
;; this should give 0
;; (reduce + (filter #(p %) (take-while #(< % limit) primes)))

;; zzZZz zzzz zzzZZZZZzz

(defn p [n] 
  (not-any? #(= n (+ (nth % 0) (nth % 1))) 
	       (selections (take-while #(< % n) abundant-numbers) 2)))

(def an (take-while #(< % limit) abundant-numbers))
;; (count an)
;; 6965
;; user> (* 6965 6965)
;; 48511225
;;(take 10 (map #(+ (nth % 0) (nth % 1)) (selections an 2)))

(def sum-of-two-abundant-numbers (into #{} (map #(+ (nth % 0) (nth % 1)) (selections an 2))))
;; (time (reduce + (filter #(not (contains? sum-of-two-abundant-numbers %)) (range 1 limit))))
;;"Elapsed time: 49.536761 msecs"
;;395465626
;; -- seen before, not it.

;; user> (time (reduce + (filter #(not (contains? sum-of-two-abundant-numbers %)) (range 1 28123))))
;; "Elapsed time: 36.964488 msecs"
;; 395437503
;; wrong again
;; user> (reduce + (range 1 28123))
;; 395437503
;; big fucking wonder .. 

;;user>  (filter #(not (contains? sum-of-two-abundant-numbers %)) (range 1 30))
;;(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29)
;; user> (contains? sum-of-two-abundant-numbers 24)
;;false

;(def s2a (into #{} (map #(+ (nth % 0) (nth % 1)) (selections an 2))))
;;Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
(def an1 (take-while #(<= % 100) abundant-numbers))
(def s21 (into #{} (filter #(<= % 100) (map #(+ (nth % 0) (nth % 1)) (selections an1 2)))))

;; (count (filter #(contains? s21 %) (range 1 100)))
;; 34
;; (count (filter #(not (contains? s21 %)) (range 1 100)))
;; 65

(comment
  (def an (take-while #(<= % limit) abundant-numbers))
  (def s2 (into #{} (filter #(<= % limit) (map #(+ (nth % 0) (nth % 1)) (selections an 2)))))
;; out of memory
  (def not-s2 (filter #(not (contains? s21 %)) (range 1 limit)))
;; (count an)
;; 6965
  (count s2)
  (count not-s2)
)
;;;............................................................................

(defn divisors [n] (set (map #(reduce * %) (subsets (factors n)))))
(defn sum-of-proper-divisors [n] (- (reduce + (divisors n)) n))
(defn abundant [n] (> ( sum-of-proper-divisors n) n))
(def abundant-numbers (filter abundant (iterate inc 1)))
(def limit 28123)

;; lazy sequence, that gives the sums of abundant numbers
(def asums (map #(+ (nth % 0) (nth % 1)) (selections abundant-numbers 2)))
;; problem is, that there are a lot (* 6965 6965) 48511225 which is way
;; more combinations than there are numbers under the limit given

;; creating the full list of numbers under the limit, that are asums
;; should be possible .. but takes a while
(def asums-set (into #{} (filter #(<= % limit) asums)))

;; a lot of the combinations are not necesary, maybe we can keep under the limit as we go?
(def asums (map #(+ (nth % 0) (nth % 1)) 
		(filter #(<= (+ (nth % 0) (nth % 1)) limit)
			(selections abundant-numbers 2))))
;; should only give the the numbers under the limit that are sums of two abundant numbers
;;(def asums-set (into #{} (filter #(<= % limit) asums)))
;; this also takes a while, and runs out of memory

;;user> (count (filter odd? (take-while #(<= % limit) abundant-numbers)))
;;62

;;user> (count (filter prime? (filter odd? (take-while #(<= % limit) abundant-numbers))))
;;0


