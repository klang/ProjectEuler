(ns problem087
  (:use [clojure.contrib.math]
	[clojure.set :only (difference union)]
	[clojure.test])
  (:use [tools.primes :only (prime?)]))
;; The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28. In fact, there are exactly four numbers below fifty that can be expressed in such a way:
;; 
;; 28 = 2^2 + 2^3 + 2^4
;; 33 = 3^2 + 2^3 + 2^4
;; 49 = 5^2 + 2^3 + 2^4
;; 47 = 2^2 + 3^3 + 2^4
;; How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?



(defn sum-of-square-cube-fourth-power [a b c]
;  (do (println (list a b c (+ (expt a 2) (expt b 3) (expt c 4))))) 
  (+ (expt a 2) (expt b 3) (expt c 4)))

(defn sum-under [a b c limit]
  (< (sum-of-square-cube-fourth-power a b c) limit))

;; (first (reverse (take-while #(sum-under % 0 0 limit) (iterate inc 1))))
;; 7071
;; (first (exact-integer-sqrt limit))
;; 7071
;; (int (expt limit 1/3))
;; 368
;; (first (exact-integer-sqrt al))
;; 84

;; user> (* 7071 368 84)
;; 218578752

;; definately not practial to do in the naïve way ..

(defn naïve [limit]
  (let [al (first (exact-integer-sqrt limit))
	bl (int (expt limit 1/3))
	cl (first (exact-integer-sqrt al))]
    (do (println (str (* al bl cl) " calculations")))
    (into #{} (for [a (range 0 (+ al 1)) 
		    b (range 0 (+ bl 1)) 
		    c (range 0 (+ cl 1))] 
		  (let [s (sum-of-square-cube-fourth-power a b c)] (if (< s limit) s 0))))))

;; if a b or c are at the limits, most of the other terms can be ignored
;; i.e. if a is at limit, b only needs to run up to 9 (if c is zero)
; (first (reverse (take-while #(< (expt % 3) (- limit (expt 7071 2))) (iterate inc 1))))
; and c only needs to run up to 5 (if b is zero)
; (first (reverse (take-while #(< (expt % 4) (- limit (expt 7071 2))) (iterate inc 1))))

; (def expt (memoize expt))
; (def limit 50000000)
(comment
  (def al (first (exact-integer-sqrt limit)))
  (def bl (expt limit 1/3))
  (def cl (first (exact-integer-sqrt al))))

;; o-------------------------------o---------------------------o----------------o 

(defn smart [limit]
  (let [l (first (exact-integer-sqrt limit))]
    (for [a (range l -1 -1)]
      (let [a2 (expt a 2), l2 (int (expt (- limit a2) 1/3))]
	(for [b (range l2 -1 -1)]
	  (let [b3 (expt b 3), l3 (int (expt (- limit b3 a2) 1/4))]
	    (for [c (range l3 -1 -1)] 
	      (+ a2 b3 (expt c 4))
	      )))))))

(defn smarter [limit]
  (let [l (first (exact-integer-sqrt limit))]
    (for [a (range l -1 -1)
	  b (range (int (expt (- limit (expt a 2)) 1/3)) -1 -1)
	  c (range (int (expt (- limit (expt b 3) (expt a 2)) 1/4)) -1 -1)
	  ]
      (+ (expt a 2) (expt b 3) (expt c 4))
      )))

;;(count (into #{} (smarter 500)))

;; the smart and smarter ways to count do not match the naïve way

;; PRIMES!! prime squares .. argh!

(comment
  (* (count (take-while #(< % (first (exact-integer-sqrt 50000000))) primes))
     (count (take-while #(< % (expt 50000000 1/3)) primes))
     (count (take-while #(< % (expt 50000000 1/4)) primes))))

;; 1524532

;; maybe the naïve way is practical, if we start by correcting the limits and limiting to primes

(defn naïve [limit]
  (let [al (first (exact-integer-sqrt limit))
	bl (int (expt limit 1/3))
	cl (first (exact-integer-sqrt al))]
    ;(do (println (str (* al bl cl) " calculations")))
    (into #{} (for [a (range 2 (+ al 1)) 
		    b (range 2 (+ bl 1)) 
		    c (range 2 (+ cl 1)) :when (and (prime? a) (prime? b) (prime? c) )] 
		  (let [s (sum-of-square-cube-fourth-power a b c)] (if (< s limit) s 0))))))

;; (naïve 50)
;;#{0 33 47 49 28}
;; 0 is added to the set for cases where s < limit, so decrese count by one 

(dec (count (naïve 50)))

;; well, at least that count is correct 
;;user> (time (dec (count (naïve 500000))))
;;"Elapsed time: 44634.654071 msecs"
;;18899

;; well, that's very close to the cut off time and we are not even close..
;; The smart methods might have to be pulled down anyway

(defn smarter [limit] 0)

(deftest test-smarter
  (is (= #{0} (difference (naïve 50) (into #{} (smarter 50)))))
  (is (= #{0} (difference (naïve 500) (into #{} (smarter 500)))))
  (is (= #{0} (difference (naïve 5000) (into #{} (smarter 5000)))))
  (is (= 4 (count (smarter 50))))
  (is (= 53 (count (into #{} (smarter 500)))))
  (is (= 395 (count (into #{} (smarter 5000)))))
  ;(is (= 2579 (count (smarter 50000))))
  ;(is (= 18899 (count (smarter 500000))))
  )

(defn smarter [limit]
  (let [l (first (exact-integer-sqrt limit))]
    (for [a (range l 1 -1)
	  b (range (int (expt (- limit (expt a 2)) 1/3)) 1 -1)
	  c (range (int (expt (- limit (expt b 3) (expt a 2)) 1/4)) 1 -1)
	  :when (and (prime? a) (prime? b) (prime? c) )
	  ]
      (+ (expt a 2) (expt b 3) (expt c 4))
      )))

;; user> (time (count (into #{} (smarter 500000))))
;; "Elapsed time: 31859.477963 msecs"
;; 18899

;; not much gained there

(defn smart [limit]
  (let [l (first (exact-integer-sqrt limit))]
    (for [a (range l -1 -1) :when (prime? a)]
      (let [a2 (expt a 2), l2 (int (expt (- limit a2) 1/3))]
	(for [b (range l2 -1 -1)  :when (prime? b)]
	  (let [b3 (expt b 3), l3 (int (expt (- limit b3 a2) 1/4))]
	    (for [c (range l3 -1 -1) :when (prime? c)] 
	      (+ a2 b3 (expt c 4)))))))))

;; (time (count (smart 500000)))
;; "Elapsed time: 35.330763 msecs"

;; there is hope here, if we can unwrap the result and count it..
(deftest test-smart
  (is (= 4 (reduce + (map #(reduce + %) (map (fn[a] (map (fn[b] (count b)) a)) (smart 50)))))))
;; though correct and cool, this does count duplicates too..

; lets fold the results as we go
(defn smartest [limit]
  (let [l (first (exact-integer-sqrt limit))]
    (reduce union  
	    (for [a (range l -1 -1) :when (prime? a)]
	      (let [a2 (expt a 2), l2 (int (expt (- limit a2) 1/3))]
		(reduce union 
			(for [b (range l2 -1 -1)  :when (prime? b)]
			  (let [b3 (expt b 3), l3 (int (expt (- limit b3 a2) 1/4))]
			    (into #{} (for [c (range l3 -1 -1) :when (prime? c)] 
					(+ a2 b3 (expt c 4))))))))))))

(deftest test-smarter
  (is (= #{0} (difference (naïve 50) (smartest 50))))
  (is (= #{0} (difference (naïve 500) (smartest 500))))
  (is (= #{0} (difference (naïve 5000) (smartest 5000))))
  (is (= 4 (count (smarter 50))))
  (is (= 53 (count (smartest 500))))
  (is (= 395 (count (smartest 5000))))
  (is (= 2579 (count (smartest 50000))))
  (is (= 18899 (count (smartest 500000))))
  )

;; user> (time (count (smartest 500000)))
;; "Elapsed time: 1438.981405 msecs"
;; 18899

;;user> (time (count (smartest 5000000)))
;;"Elapsed time: 11905.229344 msecs"
;;138932

;; .. it's going to be close and we could probably elliminate some of the fourth root calculations

;; user> (time (count (smartest 50000000)))
;; "Elapsed time: 125409.146894 msecs"
;; 1097343

;; (int (expt (- limit b3 a2) 1/4))
;; like this
;; (first (exact-integer-sqrt (first (exact-integer-sqrt (- limit b3 a2)))))
;;

(defn smartest [limit]
  (let [l (first (exact-integer-sqrt limit))]
    (reduce union  
	    (for [a (range l -1 -1) :when (prime? a)]
	      (let [a2 (expt a 2), l2 (int (expt (- limit a2) 1/3))]
		(reduce union 
			(for [b (range l2 -1 -1)  :when (prime? b)]
			  (let [b3 (expt b 3), 
				l3 (first (exact-integer-sqrt (first (exact-integer-sqrt (- limit b3 a2)))))]
			    (into #{} (for [c (range l3 -1 -1) :when (prime? c)] 
					(+ a2 b3 (expt c 4))))))))))))

;; user> (time (count (smartest 50000000)))
;; "Elapsed time: 113452.057363 msecs"
;;1097343

;; .. well, it didn't give much ..

;; I should probably have started by taking a, b and c from a prime sequence

(defn problem087 [] (count (smartest 50000000)))