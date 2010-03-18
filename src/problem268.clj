(load "tools")
(use 'clojure.contrib.combinatorics)

;; It can be verified that there are 23 positive integers less than 1000 that are divisible by at least four distinct primes less than 100.

;;  Find how many positive integers less than 10^16 are divisible by at least four distinct primes less than 100.

(defn distinct-factors [n]
  (count (distinct (prime-factors n))))

(defn distinct-factors-less-than-100 [n]
  (count (filter #(< % 100) (set (prime-factors n)))))

;; (reduce #(< 4 %) (map #(distinct-factors-less-than-100 %) (range 1 1001)))

(defn problem268 [n]
  (count (filter #(<= 4 % ) 
		 (map #(distinct-factors-less-than-100 %) 
		      (range 1 n)))))

(deftest test-problem268
  (is (= 23 (problem268 1000))))

; (run-tests)
;; (time (problem268 (expt 10 16)))
;; runs too long

(def my-primes (take-while #(<= % 100) primes))

;; take 4 different primes from my-primes
;; filter all the products that are less than 10^16
(count (selections my-primes 4))
;; user> (count (selections my-primes 4))
;; 390625
;; one prime is allowed to be present more than once ..

;; only check even numbers .. 
;; probably not a good idea, as we are not looking for primes, 
;; but numbers that have a specific number of factors

(defn problem268factors [n]
  "returns false fast, if factors higher than 100 are found"
  (loop [pfs []
	 p (take-while #(<= % 100) primes)  ;; we only accept 'small' primes
	 number n]
    (if (or (= 0 (count p)) ;; there is no more primes to try with
	    (= number 1))   ;; number is prime and has only 1 factor
      false ;pfs            ;; this is not the number we are looking for
      (let [f (first p)]
	(if (zero? (rem number f))
	  ;; prime f is a factor of number 
	  ;; (f might still be a factor, but we do not care, use next p)
	  ;; .. come to think about it, break out and return true if 
	  ;; 3 different primefactors have already been found (making f the 4th)
	  (if (<= 3 (count pfs))
	    true ; (conj pfs f)
	    (recur (conj pfs f) (rest p) (quot number f)))
	  ;; try with the next prime
	  (recur pfs (rest p) number))))
    ))

(deftest test-problem268factors
  (is (or (= true (problem268factors (* 89 97 83 79 79 79)))
	  (= [79 83 89 97] (problem268factors (* 89 97 83 79 79 79)))))
  (is (or (= false (problem268factors (* 89 97 83 197)))
	  (= [83 89 97] (problem268factors (* 89 97 83 197)))))
  (is (or (= false (problem268factors 1))
	  (= [] (problem268factors 1)))))

;; (map #(factors %) (filter #(if (problem268factors %) %) (range 1 2000)))

(defn problem268 [n]
  (count (filter #(<= 4 % ) 
		 (map #(distinct-factors-less-than-100 %) 
		      (range 1 n)))))


(defn problem268f [n]
  (count (filter #(if (problem268factors %) %) (range 1 n))))

;; user> (time (problem268f 1000))
;; "Elapsed time: 107.056661 msecs"
;; 23
;; user> (time (problem268 1000))
;; "Elapsed time: 49.863615 msecs"

(defn problem268f [n]
  (count (filter #(problem268factors %) (range 1 n))))
;; user> (time (problem268f 1000))
;; "Elapsed time: 99.271593 msecs"
;; user> (time (problem268f 10000))
;; "Elapsed time: 919.350937 msecs"
;; 811

;; not practical

(def primes-below-100 (take-while #(<= % 100) primes))

(defn problem268factors [n]
  "returns false fast, if factors higher than 100 are found"
  (let [pfs (transient [])]
    (loop [;i   0
	   p   primes-below-100
	   number n]
      (if (or ;(< i 25)
	      (= 0 (count p)) ;; there is no more primes to try with
	      (= number 1))   ;; number is prime and has only 1 factor
	false ;pfs            ;; this is not the number we are looking for
	(let [f (first p)]
	  (if (zero? (rem number f))
	    ;; prime f is a factor of number 
	    ;; (f might still be a factor, but we do not care, use next p)
	    ;; .. come to think about it, break out and return true if 
	    ;; 3 different primefactors have already been found (making f the 4th)
	    (if (<= 3 (count pfs))
	      true			; (conj pfs f)
	      (do  (conj! pfs f)  (recur 
				   ;(inc i) 
				   (rest p) (quot number f))))
	    ;; try with the next prime
	    (recur 
	     ;(inc i) 
	     (rest p) number)))))))

;; stil not practical to make any kind of calculation at each point in the range ..

;; user> (time (count (range 1 (expt 10 7))))
;; "Elapsed time: 7521.258606 msecs"
;; 9999999
