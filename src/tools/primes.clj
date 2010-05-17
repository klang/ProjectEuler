(ns tools.primes
  (:use [clojure.contrib.combinatorics])
  (:use [clojure.contrib.lazy-seqs :only (primes)]))

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

(defn factors [n]
  (loop [pfs []
	 p primes ; (take-while #(<= (* % %) n) primes)
	 number n
	 ]
; done if n=1
; otherwise, if there are no more primes and n>1, then the n is prime.. what?
; no more primes under (sqrt n) and no factors found => n is prime
    (if (= number 1)
      pfs
      (let [f (first p)]
	(if (zero? (rem number f))
	  ;; prime f is a factor of number 
	  ;; (f might still be a factor, so keep the same p)
	  (recur (conj pfs f) p (quot number f))
	  ;; try with the next prime
	  (recur pfs (rest p) number))))))


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
  (cond (= n 1) 1
	(< n 1) nil
	:else
	(* n (reduce * (map #(- 1 (/ 1 %)) (distinct (prime-factors n)))))))

(defn totient [n]
  "more efficient, totient(1)=1 is not returned"
  (* n (reduce * (map #(- 1 (/ 1 %)) (distinct (prime-factors n))))))

(def totient-seq (lazy-cat (list 1) (map #(totient %) (iterate inc 2))))

;; --- let's try to build a map of totient results

; n=p1^e1*p2^e2...p1^e1, φ(n)=n(1-1/p1)(1-1/p2)...(1-1/pk).
; n=p1*p2
; φ(p1*p2)=p1*p2*(1-1/p1)(1-1/p2)=(p1-1)(p2-1)

(comment
  (meta {:hint "... the number of primes below n, pi(n)~Li(n) [Gauss' Li function] 
Li(n) = n/ln(n) + n/[ln(n)]2 + 2n/[ln(n)]3 + ... + k!n/[ln(n)]k + 1 + ... (to infinity) 
Hence n/ln(n) is a first approximation, n/ln(n) + n/[ln(n)]2 is better, and so on. 
In fact, for relatively small n, n/(ln(n)-1) turns out to be a better approximation. "
	 :optimations "return n-1 if n is prime" "return n*phi(n) if n is square."
	 }))


;; http://fupeg.blogspot.com/2009/11/clojures-primes-shootout.html
(defn sieve [n]
  (let [n (int n)]
    "Returns a list of all primes from 2 to n"
    (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
      (loop [i (int 3)
             a (int-array n)
             result (list 2)]
        (if (>= i n)
          (reverse result)
          (recur (+ i (int 2))
                 (if (< i root)
                   (loop [arr a
                          inc (+ i i)
                          j (* i i)]
                     (if (>= j n)
                       arr
                       (recur (do (aset arr j (int 1)) arr)
                              inc
                              (+ j inc))))
                   a)
                 (if (zero? (aget a i))
                   (conj result i)
                   result)))))))

(defn primes-up-to [n]
  (let [n (int n)]
    "Returns a list of all primes from 2 to n"
    (let [root (int (Math/round (Math/floor (Math/sqrt n))))]
      (loop [i (int 3)
             a (int-array n)
             result (transient [2])]
        (if (>= i n)
          (persistent! result)
          (recur (+ i (int 2))
                 (if (< i root)
                   (loop [arr a
                          inc (+ i i)
                          j (* i i)]
                     (if (>= j n)
                       arr
                       (recur (do (aset arr j (int 1)) arr)
                              inc
                              (+ j inc))))
                   a)
                 (if (zero? (aget a i))
                   (conj! result i)
                   result)))))))

(defn lazy-primes []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                (dissoc candidate)
                (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate
                (lazy-seq (next-primes (next-sieve sieve candidate)
                            (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))

;; problem214> (time (def p0 (sieve 10000000)))
;; "Elapsed time: 4428.129716 msecs"
;; problem214> (time (def p1 (primes-up-to 10000000)))
;; "Elapsed time: 2632.297053 msecs"
