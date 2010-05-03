(ns problem043
  (:use tools.numbers)
  (:use tools.primes)
  (:use [clojure.contrib.lazy-seqs :only (primes)])
  (:use clojure.contrib.combinatorics)
  (:use clojure.contrib.math)
  (:use clojure.test))

(def pandigital (permutations '(0 1 2 3 4 5 6 7 8 9)))
;; user> (factorial 10)
;; 3628800
;; minus the ones that have 0 in the first position
;; (- (factorial 10) (factorial 9))

;; the easy way to convert back to a number
;(defn integer [l] 
;  ;; alternatively use BigInteger 
;  (Integer. (apply str l)))

(partition 3 1 (rest '(1 2 3 4 5 6 7 8 9 0)))
(take 5 (map #(partition 3 1 (rest %)) pandigital))
(def test-number-t1 (partition 3 1 (rest (digits 1406357289))))
(def test-number-t2 (partition 3 1 (rest (digits 1460357289))))
(def test-number-f1 (partition 3 1 (rest (digits 1460537289))))

(defn divisible [num div]
  (= 0 (rem num div)))

(defn special-property [n]
  (map #(divisible (integer %1) %2) n [2 3 5 7 11 13 17]))

;; (reduce #(and %1 %2) (special-property test-number-f1))

;;
;; (reduce #(and %1 %2) (repeat false))
;; -- will never finish, eventhough the first term is false

;; (every? #(and true %) (repeat false))
;; (every? #(and true %) (special-property test-number-f1))

(defn special-property? [n]
  (every? #(and true %) (map #(divisible (integer %1) %2) n primes)))

(defn special-property? [n]
  (every? #(and true %) (map #(divisible (integer %1) %2) (partition 3 1 (rest n)) primes)))

;; (special-property? (digits 1406357289))

;; (reduce + (filter #(special-property? %) pandigital))

(defn special-property? [n]
  (every? true? (map #(= 0 (rem (integer %1) %2)) 
		     (partition 3 1 (rest n)) 
		     primes)))

(def known  '(1406357289 1430952867 1460357289))

(defn controlled-loop []
  (let [primes '(2 3 5 7 11 13 17)]
    (loop [pandigital pandigital
	   ;limit 1000
	   catch []]
      (if (= '() pandigital);(= 0 limit)
	catch
	(let [p (first pandigital)
	      s (special-property? p)]
	  (do (if s (println p))
	    (recur (rest pandigital) (if s (conj catch s) catch))))))))
;; runs out of heap space for some reason..

;; let's not do this brute force then.

;; we know that the numbers have to be 
;; larger than  1023456789 the smallest pandigital number and
;; smaller than 9876543210 the largest pandigital number.
;; we also know that it has to divide 17 (limiting the posibilties a bit)

;; (range 1023456789 9876543210 17)
;; (quot  (- 9876543210 1023456789) 17)
;; 520769789
;; even that range is too long to simply brute force, or do much calculation
;; at each point

;; the first number that divides 17, is 1023456797, not pandigital

(defn controlled-loop []
  ;; divisiblity by 17 can be checked by starting using the 
  ;; --> (range 1023456797 9876543210 17)
  ;; --> and only taking the numbers that have different digits
  ;; --> or simply the ones where (= 10 (count (distinct (digits n))))
  ;; d4 has to be even, for the first group to be divisible by 2 
  ;; --> (even? (nth (digits n) 3))
  ;; d5 has to be 0 or 5, for the 3rd group to be divisible by 5 
  ;; --> (or (zero? (nth (digits n) 4)) (= 5 (nth (digits n) 4)))
  (loop [numbers (range 1023456797 9876543210 17)
	 catch []]
    (let [the-digits (digits (first numbers))]
      (if (and (= 10 (count (distinct the-digits)))    ; divides 17 + is pandigital 
	       (even? (nth the-digits 3))              ; divides 2
	       (or (= 0 (nth the-digits 4)) 
		   (= 5 (nth the-digits 4)))           ; divides 5
	       (zero? (rem (quot (rem (first numbers) 10000) 10) 13)) ;; divides 13
	       (zero? (rem (quot (rem (first numbers) 100000) 100) 11)) ;; divides 11
	       (zero? (rem (quot (rem (first numbers) 1000000) 1000) 7)) ;; divides 7
	       ;(zero? (rem (quot (rem (first numbers) 10000000) 10000) 5)) ;; divides 5
	       (zero? (rem (quot (rem (first numbers) 100000000) 100000) 3)) ;; divides 3
	       ;(zero? (rem (quot (rem (first numbers) 1000000000) 1000000) 2)) ;; divides 2
	       )
	;; this is a potential candidate
	(do (println (str "first found: "(first numbers) 
			  " after " (quot (- (first numbers) 1023456797) 17) " recursions"))	    
	    )
	;; no catch, let's try the next
	(recur (rest numbers) catch )))))

;; too slow .. there are too many numbers generated, and too many calculations done to cut it down

;; ---
;; back to the start

(defn special-property? [n]
  (every? true? (map #(= 0 (rem (integer %1) %2)) 
		     (partition 3 1 (rest n)) 
		     [2 3 5 7 11 13 17])))

;; the first pandigital number without a leading zero is 1023456789
;; as permutations give the results in order, that will be the (nth pandigital (factorial9))'th number
;; so we can safely drop anything before that

(def pandigital (drop (factorial 9) (permutations '(0 1 2 3 4 5 6 7 8 9))))

;; http://en.wikipedia.org/wiki/Divisibility_rule
;; the special property can be reduced further, as we know that
;; d4 has to be even, for the first group to be divisible by 2 
;; --> (even? (nth (digits n) 3))
;; d5 has to be 0 or 5, for the 3rd group to be divisible by 5 
;; --> (or (zero? (nth (digits n) 4)) (= 5 (nth (digits n) 4)))
(def pandigital (filter #(and (even? (nth % 3))
			      (or (= 5 (nth % 5)) 
				  (= 0 (nth % 5)))) 
			(drop (factorial 9) (permutations '(0 1 2 3 4 5 6 7 8 9)))))
;; problem043> (time (first pandigital))
;; "Elapsed time: 2220.100732 msecs"
;; (1 0 2 4 3 5 6 7 8 9)

(defn controlled-loop []
  (let [primes '(2 3 5 7 11 13 17)]
    (loop [pandigital pandigital
	   catch []
	   i 0]
      (if (= '() pandigital)
	catch
	(let [p (first pandigital)
	      s (special-property? p)]
	  (do (if s (println (list p i)))
	    (recur (rest pandigital) (if s (conj catch (integer p)) catch) (inc i))))))))

; the version of pandigital, that just drops 9! members
;((1 4 0 6 3 5 7 2 8 9) 123270)
;((1 4 3 0 9 5 2 8 6 7) 131668)
;((1 4 6 0 3 5 7 2 8 9) 141270)

; the versionthat also checks for divisibility by 2 and 5
;((1 4 0 6 3 5 7 2 8 9) 14550)
;((1 4 3 0 9 5 2 8 6 7) 15460)
;((1 4 6 0 3 5 7 2 8 9) 16590)

;; problem043> (time (controlled-loop))
;; ((1 4 0 6 3 5 7 2 8 9) 14550)
;; ((1 4 3 0 9 5 2 8 6 7) 15460)
;; ((1 4 6 0 3 5 7 2 8 9) 16590)
;; ((4 1 0 6 3 5 7 2 8 9) 128310)
;; ((4 1 3 0 9 5 2 8 6 7) 129220)
;; ((4 1 6 0 3 5 7 2 8 9) 130350)
;; "Elapsed time: 71378.25859 msecs"
;; [true true true true true true]

;; execution time acceptable
;(+ 1406357289 1430952867 1460357289 4106357289 4130952867 4160357289)

;; problem043> (time (reduce + (map integer (filter #(special-property? %) pandigital))))
;; "Elapsed time: 72894.258687 msecs"
;; 16695334890

;; avoid doing check for divisibility by 2 too many times
(defn special-property? [n]
  (every? true? (map #(= 0 (rem (integer %1) %2)) 
		     (rest (partition 3 1 (rest n))) 
		     [3 5 7 11 13 17])))

;; problem043> (time (reduce + (map integer (filter #(special-property? %) pandigital))))
;; "Elapsed time: 16283.836793 msecs"
;; 16695334890

(defn special-property? [n]
  (let [parts (partition 3 1 (rest n))]
    (and ;(zero? (rem (integer (nth parts 0)) 2))
	 (zero? (rem (integer (nth parts 1)) 3))
	 ;(zero? (rem (integer (nth parts 2)) 5))
	 (zero? (rem (integer (nth parts 3)) 7))
	 (zero? (rem (integer (nth parts 4)) 11))
	 (zero? (rem (integer (nth parts 5)) 13))
	 (zero? (rem (integer (nth parts 6)) 17)))))

;; problem043> (time (reduce + (map integer (filter #(special-property? %) pandigital))))
;; "Elapsed time: 13275.54887 msecs"
;; 16695334890

;; the time can probably be squeezed further by using the following ideas

(defn divides-7 [n]
  "Subtract 2 times the last digit from the rest.")
(defn divides-11 [n]
  "Subtract the last digit from the rest." )
(defn divides-13 [n]
  "Add 4 times the last digit to the rest." )
(defn divides-17 [n]
  "Subtract 5 times the last digit from the rest"
  (= 17 (- (* 5 (rem n 10)) (quot n 10) )))



