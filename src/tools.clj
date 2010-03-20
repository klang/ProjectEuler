(use 'clojure.contrib.test-is 
     '[ clojure.contrib.lazy-seqs :only (primes)]
     '[clojure.contrib.math :only (expt sqrt exact-integer-sqrt)]
     '[clojure.contrib.str-utils2 :only (split)]
     'clojure.contrib.math
     'clojure.contrib.repl-utils
     'clojure.contrib.combinatorics
     'clojure.set)


(defn digits [number]
  (map #(. Integer parseInt % 10) 
	     (filter #(not (= % "")) (split (str number) #""))))

(defn reverse-number [number]
  (loop [n number dl 0]
    (if (zero? n) dl
      (recur (quot n 10) (+ (* 10 dl) (rem n 10))))))
(defn palindrome? [n] (= (reverse-number n) n))

(defn digit-list [number]
  "convert number to digit list"
  (cond (zero? number) (list 0)
	:else
	(loop [n number dl ()]
	  (if (zero? n) dl
	      (recur (quot n 10) (conj dl (rem n 10)))))))

(defn digits [number] (digit-list number))

(defn digit-set [number]
  (loop [n number dl #{}]
    (if (zero? n) dl
      (recur (quot n 10) (conj dl (rem n 10))))))

(defn digit-list2number [digit-list]
  (reduce + (map #(* %1 (expt 10 %2)) 
		 (reverse digit-list) 
		 (range 0 (count digit-list)))))

(defn integer [digit-list]
  (digit-list2number digit-list))

(defmulti digits-odd? class)
(defmethod digits-odd? clojure.lang.LazySeq [list] 
  (every? odd? list))
(defmethod digits-odd? java.lang.Integer [number] 
  (every? odd? (digits number)))


(defn factorial [n] 
  (reduce * (range n 0 -1)))

(defn factorials []
  (map first (iterate (fn [[a b]] [(* a b) (inc b)]) [1 2])))

(defn fibos []
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

; though interesting, there really is no point in using the above implementation
(def factors prime-factors)



;; prime-factors has an assert that I keep falling in when using prime? in a functional way
(defn prime? [n]
  (if (>= 1 n)
    false
    (= 1 (count (prime-factors n)))))

(defn divisors# [n]
  (if (>= 1 n)
    1
    (count (set (map #(reduce * %) (subsets (prime-factors n)))))))

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

; http://en.wikipedia.org/wiki/Divisor_function
; Sigma(p^n) = [p^(n+1)]-1/(p-1)

(defn amicable-pair [a b]
  (and (not (= a b) (prime? a) (prime? b)) 
       (= (sum-of-proper-divisors a) b) 
       (= (sum-of-proper-divisors b) a)))

;; http://groups.google.com/group/k12.ed.math/browse_thread/thread/19f74d278e88b65d/bd50b5ae25c74465?lnk=st&q=computing+euler+totient+function&rnum=4&pli=1
;; translated from scheme
(defn rec-totient [n]
  (loop [tot 0, pos (- n 1)]
    (if (> pos 0)
      (if (= 1 (gcd n pos))
	(recur (+ tot 1) (- pos 1))
	(recur tot (- pos 1)))
      tot)))

(defn totient [n]
  "totient(n) = n * (1 - 1/p1)(1 - 1/p2)(1 - 1/p3)...(1 - 1/pm) 
where p1...pm are the unique prime factors of n."
  (* n (reduce * (map #(- 1 (/ 1 %)) (set (prime-factors n))))))

(defn totient [n]
  "totient(n) = n * (1 - 1/p1)(1 - 1/p2)(1 - 1/p3)...(1 - 1/pm) 
where p1...pm are the unique prime factors of n."
  (* n (reduce * (map #(- 1 (/ 1 %)) (distinct (prime-factors n))))))
