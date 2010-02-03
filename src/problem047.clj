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

(defn concecutive []
  )

(defn not-prime? [n]
  (not (prime? n)))

(defn not-prime? [n]
  (< 1 (count (prime-factors n))))

(take 10 (iterate #(not-prime? %) 2))

(use 'clojure.contrib.seq-utils)