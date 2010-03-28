(ns tools.primes
  (:use [clojure.contrib.combinatorics]))
(use '[clojure.contrib.lazy-seqs :only (primes)])

(def prime-gen
     (let [primes (atom [])]
       (for [n (iterate inc 2)
             :when (not-any? #(zero? (rem n %))
                             (filter #(<= % (Math/sqrt n)) 
                                     @primes))]
         (do (swap! primes conj n)
             n))))

(defn prime-factors [arg]
  (assert (and (integer? arg) (>= arg 2)))
  (loop [pfs [], n arg]  ; pfs is the vector of prime factors already determined
    (if (= n 1)
      pfs
      (let [dps (for [p primes :while (<= (* p p) n) :when (zero? (rem n p))] p)
            ps  (for [p dps, q (rest (iterate #(/ % p) n)) :while (integer? q)] p)]
        (if (empty? dps)
          (recur (conj pfs n), 1)
          (recur (into pfs ps), (apply / n ps)))))))

;; prime-factors has an assert that I keep falling in when using prime? in a functional way
(defn prime? [n]
  (if (>= 1 n)
    false
    (= 1 (count (prime-factors n)))))

(defn divisors# [n]
  (if (>= 1 n)
    1
    (count (set (map #(reduce * %) (subsets (prime-factors n)))))))

;; though using subsets does work for n with a low number of prime-factors, it will go seriously
;; wrong, if then number of prime-factors is high.

;; http://mathworld.wolfram.com/Divisor.html term number (5) says
;; the total number of divisors is given by
;; d(n) = (1+m1)(1+m2)...(1+mk), where n = p1^m1 = p2^m2 * .. * pk^mk is the prime factorization of n

(defn- mk [m coll]
  (+ 1 (count (filter #(= m %) coll))))

(defn divisors# [n]
  (let [facts (prime-factors n), divs (distinct facts) ]
    (reduce * (map #(mk % facts) divs))))

(defn divisors [n]
  (if (>= 1 n)
      #{1}
      (set (map #(reduce * %) (subsets (prime-factors n))))))

(defn proper-divisors [n]
  (disj (divisors n) n))

(defn proper-divisors# [n]
  (cond (prime? n) 1
	:else
	(count (disj (set (map #(reduce * %) (subsets (prime-factors n)))) n))))

(defn sum-of-proper-divisors [n] (reduce + (proper-divisors n)))

(defn amicable-pair [a b]
  (and (not (= a b) (prime? a) (prime? b)) 
       (= (sum-of-proper-divisors a) b) 
       (= (sum-of-proper-divisors b) a)))

;; http://en.wikipedia.org/wiki/Divisor_function
;; Sigma(p^n) = [p^(n+1)]-1/(p-1)

(defn totient [n]
  "totient(n) = n * (1 - 1/p1)(1 - 1/p2)(1 - 1/p3)...(1 - 1/pm) 
where p1...pm are the unique prime factors of n."
  (* n (reduce * (map #(- 1 (/ 1 %)) (distinct (prime-factors n))))))

;; --- let's try to build a map of totient results

; n=p1^e1*p2^e2...p1^e1, φ(n)=n(1-1/p1)(1-1/p2)...(1-1/pk).
; n=p1*p2
; φ(p1*p2)=p1*p2*(1-1/p1)(1-1/p2)=(p1-1)(p2-1)
