(ns tools.scratch)

(def prime-gen
     (let [primes (atom [])]
       (for [n (iterate inc 2)
             :when (not-any? #(zero? (rem n %))
                             (filter #(<= % (Math/sqrt n)) 
                                     @primes))]
         (do (swap! primes conj n)
             n))))

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

;; http://groups.google.com/group/k12.ed.math/browse_thread/thread/19f74d278e88b65d/bd50b5ae25c74465?lnk=st&q=computing+euler+totient+function&rnum=4&pli=1
;; translated from scheme
(defn rec-totient [n]
  (loop [tot 0, pos (- n 1)]
    (if (> pos 0)
      (if (= 1 (gcd n pos))
	(recur (+ tot 1) (- pos 1))
	(recur tot (- pos 1)))
      tot)))
