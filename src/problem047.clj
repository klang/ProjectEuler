;;The first two consecutive numbers to have two distinct prime factors are:

;;14 = 2 * 7
;;15 = 3 * 5

;;The first three consecutive numbers to have three distinct prime factors are:

;;644 = 2² * 7 * 23
;;645 = 3  * 5  * 43
;;646 = 2  * 17 * 19

;;Find the first four consecutive integers to have four distinct primes factors. What is the first of these numbers?

(load "tools")
(use 'clojure.contrib.repl-utils)

(defn distinct-prime-factors [n]
  (count (set (prime-factors n))))

(defn fibos []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(defn not-primes []
  (filter #(not (prime? %)) (iterate inc 2)))

(defn not-prime? [n]
  (not (prime? n)))

(defn not-prime? [n]
  (< 1 (count (prime-factors n))))

(use 'clojure.contrib.seq-utils)

(defn concecutive2 [[a b]]
  (< a b (+ a 2)))
(take 5 (filter concecutive (partition 2 1 (not-primes))))
(set! *print-length* 103)

(defn concecutive3 [[a b c]]
  (< a b c (+ a 3)))
(filter concecutive (partition 3 1 (not-primes)))

(defn find-target2 []
  (loop [part (filter concecutive2 (partition 2 1 (not-primes)))
	 ]
    (if (every? #(= 2 (distinct-prime-factors %)) (first part))
      (first part)
      (recur (rest part)))))

(defn find-target3 []
  (loop [part (filter concecutive2 (partition 3 1 (not-primes)))
	 ]
    (if (every? #(= 3 (distinct-prime-factors %)) (first part))
      (first part)
      (recur (rest part)))))

(defn concecutive [[a b c d]]
  (< a b c d (+ a 4)))

(defn find-target []
  (loop [part (filter concecutive (partition 4 1 (not-primes)))
	 ]
    (if (every? #(= 4 (distinct-prime-factors %)) (first part))
      (first part)
      (recur (rest part)))))

;; user> (time (find-target))
;; "Elapsed time: 22263.70793 msecs"
;; (134043 134044 134045 134046)


