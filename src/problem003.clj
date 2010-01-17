; The prime factors of 13195 are 5, 7, 13 and 29.

; What is the largest prime factor of the number 600851475143 ?

(use 'clojure.contrib.test-is 'clojure.contrib.lazy-seqs)

(def prime-gen
     (let [primes (atom [])]
       (for [n (iterate inc 2)
             :when (not-any? #(zero? (rem n %))
                             (filter #(<= % (Math/sqrt n)) 
                                     @primes))]
         (do (swap! primes conj n)
             n))))
(rem 13195 5)

;; what is the difference between rem and mod?
;; is one faster than the other or is it just taste?
;; how about quot and /?

(comment 
  (mod 13195 5)
  (quot 13195 5)
  (mod 2639 7)
  (quot 2639 7)
  (mod 377 13)
  (quot 377 13)
  (mod 29 29)
)

(defn factor n
     (let [factors (atom #{})]
       (for [p (primes) :when ]
	 (do (swap! factors conj p)
	   p))))

(defn problem003 [n]
  #{5 7 13 29})

(deftest test-problem003
  (is (= #{5 7 13 29} (problem003 13195))))

; user> (time (problem004 3))
; "Elapsed time: 3624.171717 msecs"
; 906609

; (run-tests)
