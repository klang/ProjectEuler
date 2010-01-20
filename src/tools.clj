(use 'clojure.contrib.test-is 
     '[ clojure.contrib.lazy-seqs :only (primes)]
     '[clojure.contrib.math :only (expt sqrt exact-integer-sqrt)]
     '[clojure.contrib.str-utils2 :only (split)])

(defn digits [n]
  (map #(. Integer parseInt % 10) 
	     (filter #(not (= % "")) (split (str n) #""))))

(defn factorial [n] 
  (reduce * (range n 0 -1)))

(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

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
	 p primes
	 number n]
    (if (= number 1)
      pfs
      (let [f (first p)]
	(if (zero? (rem number f))
	  ;; prime f is a factor of number 
	  ;; (f might still be a factor, so keep the same p)
	  (recur (conj pfs f) p (quot number f))
	  ;; try with the next prime
	  (recur pfs (rest p) number))))))

(defn prime? [n]
  (= 1 (count (factors n))))

(defn prime? [n]
  (loop [pfs []
	 p primes
	 number n]
    (if (< 1 (count pfs)) false 
	(if (= number 1)
	  true
	  (let [f (first p)]
	    (if (zero? (rem number f))
	      (recur (conj pfs f) p (quot number f))
	      (recur pfs (rest p) number)))))))
