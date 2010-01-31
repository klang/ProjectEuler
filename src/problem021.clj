;; Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
;; If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and each of a and b are called amicable numbers.

;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

;; Evaluate the sum of all the amicable numbers under 10000.
(load "tools")

;; naive way to find number of divisors
(use 'clojure.contrib.combinatorics)
(use 'clojure.set)

(defn divisors# [n]
  (count (set (map #(reduce * %) (subsets (factors n))))))

(defn divisors [n]
  (set (map #(reduce * %) (subsets (factors n)))))

(defn propper-divisors [n]
  (disj (divisors n) n))

(defn propper-divisors# [n]
  (count (disj (set (map #(reduce * %) (subsets (factors n)))) n)))

(defn d [n] (reduce + (propper-divisors n)))

(defn amicable-pair [a b]
  (and (not (= a b)) (= (d a) b) (= (d b) a))
  )

;; (amicable-pair 220 284)
;; true

;; (def amicable-pairs {1 0, 2 1, 3 1, 4 3, ..., 8 7, ..., 220 284, ...,284 220, .. 10000 14211})

(def possible-propper-divisors (into #{} (filter #(<= % 10000) (map #(d %) (range 1 10001)))))

;; (amicable-pair 220 (d 220))
;; true
;; (amicable-pair 284 (d 284))
;; true

;; hey, let us use that small fact..
(filter #(amicable-pair % (d %)) (range 1 10001))
; ; Evaluation aborted. -- heap space?!?

(defn af? [n]
  (let [dn (d n)] (if (and (< 1 dn ) (<= dn 10000) (amicable-pair n dn)) n nil)))

;; early termination
(filter #(af? %) (range 1 10001))
;; user> (time (count (filter #(af? %) (range 1 10001))))
;; "Elapsed time: 11755.049549 msecs"
;; 10

;; (220 284 1184 1210 2620 2924 5020 5564 6232 6368)

;; not-primes ..
(filter #(not (prime? %)) (range 2 100))

(filter #(af? %) (filter #(not (prime? %)) (range 2 10001)))
;; user> (time (count (filter #(af? %) (filter #(not (prime? %)) (range 2 10001)))))
;; "Elapsed time: 11081.735013 msecs"
;; 10

;;user> (reduce + (filter #(af? %) (filter #(not (prime? %)) (range 2 10001))))
;;31626
