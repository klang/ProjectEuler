;; A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

;; 1/2	= 	0.5
;; 1/3	= 	0.(3)
;; 1/4	= 	0.25
;; 1/5	= 	0.2
;; 1/6	= 	0.1(6)
;; 1/7	= 	0.(142857)
;; 1/8	= 	0.125
;; 1/9	= 	0.(1)
;; 1/10	= 	0.1
;; Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
;; 
;; Find the value of d <  1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.

;; http://en.wikipedia.org/wiki/Repeating_decimal

;;On the one hand, the decimal representation of a rational number is ultimately periodic because it can be determined by a long division process, which must ultimately become periodic as there are only finitely many different remainders and so eventually it will find a remainder that has occurred before.
(use 'clojure.contrib.test-is 'clojure.contrib.math 'clojure.set)
(load "tools")
(defn long-divide-cycle-length [num div] 
  (loop [number    (quot num div)
	 remainder (rem num div)
	 seen      #{}
	 seenm     {}
	 res       [number "."]
	 i         1
	 ]
    (if (zero? remainder) 0
      (if (seen remainder)
	(- i (seenm remainder))
	(recur 
	 (quot (* 10 remainder) div)
	 (rem (* 10 remainder) div)
	 (conj seen remainder)
	 (assoc seenm remainder i)
	 (conj res (quot (* 10 remainder) div))
	 (inc i))))))

;; user> (reduce max (map #(long-divide-cycle-length 1 %) (range 1 1000)))
;; 982
;; incorrect (that is the length fo the cycle)
;; the 1/d with that length is d=983

(deftest test-long-divide-cycle-length
  (is (= 1 (long-divide-cycle-length 1 3)))
  (is (= 1 (long-divide-cycle-length 1 6)))
  (is (= 6 (long-divide-cycle-length 1 7)))
  (is (= 1 (long-divide-cycle-length 1 9)))
  (is (= 3 (long-divide-cycle-length 3227 555))))

(defn long-divide [num div] 
  (loop [number    (quot num div)
	 remainder (rem num div)
	 seen      #{}       ; keep remainders for easy lookup
	 seenm     {}        ; associate them with index for easy length calculation
	 res       [number]
	 i         1         ; how many remainders have we seen
	 ]
    (if (zero? remainder)
      (str (first res) (if (empty? (rest res)) ""  ".") 
	   (apply str (rest res)))
      (if (seen remainder)
       (let [before-decimal-point (first res)
	     cycle-start          (+ 1 (seenm remainder))
	     non-cycle-part       (apply str (subvec res 1 (- cycle-start 1)))
	     cycle-part           (apply str (subvec res (- cycle-start 1)))
	     return-value         (str before-decimal-point "." non-cycle-part "(" cycle-part ")")]
	(comment
	  (list (apply str res)		; result as string
		seen			; remainder set
		seenm			; remainder map
		remainder		; remainder seen twice
		res			; result as vector
		i			; decimals + 1 cycle
		(- i (seenm remainder))	; cycle has this length
		(seenm remainder)	; remaider was seen here
					; before the decimal point.
		return-value
		(subvec res 0 2)        
		(subvec res 2 (+ 1 (seenm remainder)))
		(subvec res (+ 1 (seenm remainder)))
		))
	 return-value)
       (recur 
	(quot (* 10 remainder) div)
	(rem (* 10 remainder) div)       
	(conj seen remainder)	     
	(assoc seenm remainder i)    
	(conj res (quot (* 10 remainder) div))
	(inc i)
	)))))

(deftest test-long-divide
  (is (= "0.(3)" (long-divide 1 3)))
  (is (= "0.1(6)" (long-divide 1 6)))
  (is (= "0.(142857)" (long-divide 1 7)))
  (is (= "0.(1)" (long-divide 1 9)))
  (is (= "5.8(144)" (long-divide 3227 555)))
  (is (= "1" (long-divide 1 1)))
  (is (= "0" (long-divide 0 1)))
  )

;; On the other hand, each repeating decimal number satisfies a linear equation with integral coefficients, and its unique solution is a rational number.

(defn terminating-decimal [k d]
  "Terminating decimals represent rational numbers of the form k/d. Where d = 2^n * 5^m"
  (not (empty? (intersection #{2 5} (set (factors d))))))

(defn repeating-decimal [fraction] nil)

(defn carmichael-function [n] nil)

;; http://groups.google.com/group/k12.ed.math/browse_thread/thread/19f74d278e88b65d/bd50b5ae25c74465?lnk=st&q=computing+euler+totient+function&rnum=4&pli=1
;; translated from scheme
(defn totient [n]
  (loop [tot 0, pos (- n 1)]
    (if (> pos 0)
      (if (= 1 (gcd n pos))
	(recur (+ tot 1) (- pos 1))
	(recur tot (- pos 1)))
      tot)))

(defn totient [n]
  "totient(n) = n * (1 - 1/p1)(1 - 1/p2)(1 - 1/p3)...(1 - 1/pm) 
where p1...pm are the unique prime factors of n."
  (* n (reduce * (map #(- 1 (/ 1 %)) (set (factors n))))))

(defn cyclic-number [n]
  "If the period of the repeating decimal of 1⁄p is equal to p − 1 then the repeating decimal part is called a cyclic number."
  (= (totient n) (- n 1)))

