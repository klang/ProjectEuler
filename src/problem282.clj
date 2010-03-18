;; For non-negative integers m, n, the Ackermann function A(m, n) is defined as follows:

;;
;;              A(m,n) = 
;;

;; For example A(1, 0) = 2, A(2, 2) = 7 and A(3, 4) = 125.

;; Find 0 <= n <= 6 A(n, n) and give your answer mod 14^8.

(use 'clojure.contrib.test-is
     'clojure.contrib.math)

(load "tools")

(defn A [m n]
  (cond (= m 0) (+ n 1)
	(and (< 0 m) (= n 0)) (A (- m 1) 1)
	(and (< 0 m) (< 0 n)) (A (- m 1) (A m (- n 1)))))

;; consuming stack frames like there is no tomorrow

(deftest test-Ackermann
  (is (= 2 (A 1 0)))
  (is (= 7 (A 2 2)))
  (is (= 125 (A 3 4))))

;;A(m,n)       n=0             n=1             n=2         n=3            n=4         n=5         n=6
;; m=0           1               2               3           4              5           6           7
;; m=1           2               3               4           5              6           7           8
;; m=2           3               5               7           9             11          13          15
;; m=3           5              13              29          61            125         253         509
;; m=4          13           65533        265536-3   2265536-3 A(3,2265536-3) A(3,A(4,4)) A(3,A(4,5))
;; m=5       65533      A(4,65533) A(4,A(4,65533)) A(4,A(5,2))    A(4,A(5,3)) A(4,A(5,4)) A(4,A(5,5))
;; m=6  A(4,65533) A(5,A(4,65533))     A(5,A(6,1)) A(5,A(6,2))    A(5,A(6,3)) A(5,A(6,4)) A(5,A(6,5))

;; ( 1 + 3 + 7 + 61 + A(3,2265536-3) + A(4,A(5,4)) + A(5,A(6,5)) ) mod 14^8

;; http://en.wikipedia.org/wiki/Ackermann_function

;; If we define the function f (n) = A(n, n), which increases both m and n at the same time, we have a function of one variable that dwarfs every primitive recursive function, including very fast-growing functions such as the exponential function, the factorial function, multi- and superfactorial functions, and even functions defined using Knuth's up-arrow notation (except when the indexed up-arrow is used).

;; (reduce + (map #(A % %) (range 0 4)))
;; well .. as we do not have infinite memory on this machine .. 

;; from RosettaCode.org, but not really different from what I already have.
(defn ackermann [m n] 
  (cond (zero? m) (inc n)
        (zero? n) (ackermann (dec m) 1)
        :else (ackermann (dec m) (ackermann m (dec n)))))

;; adaption of the mathematica implementation 
;; + the tail recursion idea seen in the python implementation
;; both from RosettaCode.org
(defn ack [m n] 
  (cond (= m 0) (+ n 1)
	(= m 1) (+ n 2)
	(= m 2) (+ 3 (* 2 n))
	(= m 3) (+ 5 (* 8 (- (expt 2 n) 1)))
        (= n 0) (recur (dec m) 1)
        :else (recur (dec m) (ack m (dec n)))))

;; at least we are now able to calculate higher values
(deftest test-ack
  (is (= 3 (ack 1 1)))   (is (= 4 (ack 1 2)))   (is (= 5 (ack 1 3)))    (is (= 6 (ack 1 4))) 
  (is (= 7 (ack 1 5)))   (is (= 8 (ack 1 6)))   (is (= 9 (ack 1 7)))    (is (= 10 (ack 1 8)))
  (is (= 5 (ack 2 1)))   (is (= 7 (ack 2 2)))   (is (= 9 (ack 2 3)))    (is (= 11 (ack 2 4)))
  (is (= 13 (ack 2 5)))  (is (= 15 (ack 2 6)))  (is (= 17 (ack 2 7)))   (is (= 19 (ack 2 8)))
  (is (= 13 (ack 3 1)))  (is (= 29 (ack 3 2)))  (is (= 61 (ack 3 3)))   (is (= 125 (ack 3 4)))
  (is (= 253 (ack 3 5))) (is (= 509 (ack 3 6))) (is (= 1021 (ack 3 7))) (is (= 2045 (ack 3 8)))
  (is (= 13 (ack 4 0)))  (is (= 65533 (ack 4 1))) (is (= 19729 (count (digits (ack 4 2)))))
)

;;----- obviously, the recursive definition of the Ackerman function is a dead end.

;; Using Knuth's up-arrow notation, it's possible to express the function in a better way:

(load "problem188")
;; mod-expt, hyper-mod-bin, mod-expt-bin

(deftest test-mod-expt
  (is (= 67108864 (expt 4 13)))
  (is (= 445 (mod 67108864 497)))
  (is (= 445 (mod (expt 4 13) 497)))
  (is (= 445 (mod-expt 4 13 497)))
  (is (= 445 (mod-expt-bin 4 13 497))))

; 2^^5
;(mod (expt 2 (expt 2 (expt 2 (expt 2 2)))) (expt 14 8))
;804023040
;;(mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 2 (expt 14 8)) (expt 14 8)) (expt 14 8)) (expt 14 8))
;;804023040

;(mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 2)))))

;;2^^6
;;(mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 2 (expt 14 8)) (expt 14 8)) (expt 14 8)) (expt 14 8)) (expt 14 8))
;; 1147901952

;; 2^^7
;; user> (mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 2 (expt 14 8)) (expt 14 8)) (expt 14 8)) (expt 14 8)) (expt 14 8)) (expt 14 8)) 
;; 982905344

;; user> (time (reduce #(mod-expt %2 %1 (expt 14 8)) [2 2 2 2 2 2 2]))
;; "Elapsed time: 1828699.057554 msecs"
;; 982905344
;; -- mod-expt has to be a bit more efficient, but at least the reduce form does what I want.

;; A(4,4) mod 14^8 = 982905344 - 3 = 982905341
;(mod-expt 2 1147901952 (expt 14 8))


;(hyper 2 1 4)
; 2↑4 = 2 * 2 * 2 * 2 = (expt 2 4)
;(hyper 2 2 4)
; 2↑↑4 = 2↑2↑2↑2↑2 = (expt 2 (expt 2 (expt 2 2)))
;(expt 2 (expt 2 (expt 2 2)))
;(hyper 2 3 2)
; 2↑↑↑8 = 2↑↑(2↑↑↑7) = 2↑↑(2↑↑(2↑↑↑6)) = 2↑↑(2↑↑(2↑↑(2↑↑↑5))) = 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑4)))) = 
; 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑3))))) = 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑2)))))) = 
; 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑1))))))) = 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑1))))))) =

;; hyper, adopting another name, as the 'hyper' function is defined differently traditionally
(defn up-arrow [a n b]
  (reduce #(expt %2 %1) (repeat (* (- n 1) b) a)))
;;--------------
(defn up-arrow [a n b]
  (reduce #(expt %2 %1) (repeat (* (- n 1) b) a)))

;; TODO:  the up-arrow function works only for n=2 .. for higher n, the count is wrong
(defn up-arrows [a n b]
  (repeat (* (- n 1) b) a))

(deftest test-up-arrows
  ;; example from http://en.wikipedia.org/wiki/Knuth's_up-arrow_notation
  ;; 3↑↑↑2 = 3↑↑(3↑↑↑1) = 3↑↑3 = 3↑(3↑↑2) = 3↑(3↑(3↑1)) = 3↑3↑3
  (is (= (up-arrows 3 3 2) (up-arrows 3 2 3))))

;;--------------

(deftest test-up-arrow
  (is (= 65536 (up-arrow 2 2 4) (up-arrow 2 3 2)))
  (is (= (- (expt 2 (expt 2 (expt 2 (expt 2 2)))) 3) 
	 (- (expt 2 65536) 3)
	 (- (up-arrow 2 2 5) 3)
	 (ack 4 2))))

;; hyper-mod
(defn up-arrow-mod [a n b m]
  (reduce #(mod-expt %2 %1 m) (repeat (* (- n 1) b) a)))

(deftest test-up-arrow-mod
  (is (= 65536 (up-arrow-mod 2 2 4 (expt 14 8)) (up-arrow-mod 2 3 2 (expt 14 8)))
      (= 4 (up-arrow-mod 2 2 4 7) (up-arrow-mod 2 3 2 7)))
  (is (= (- (mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 2 (expt 14 8)) 
					      (expt 14 8)) 
				  (expt 14 8)) 
		      (expt 14 8)) 3)
	 (- (mod-expt 2 65536 (expt 14 8)) 3)
	 (mod (ack 4 2) (expt 14 8))
	 (mod (- (up-arrow 2 2 5) 3) (expt 14 8))
	 (- (up-arrow-mod 2 2 5 (expt 14 8)) 3) 
	 (mod (- (expt 2 (expt 2 (expt 2 (expt 2 2)))) 3) (expt 14 8)))))

(defn up-arrow-mod-bin [a n b m]
  (reduce #(mod-expt-bin %2 %1 m) (repeat (* (- n 1) b) a)))

(deftest test-up-arrow-mod-bin
  (is (= 65536 
	 (mod (up-arrow 2 2 4) (expt 14 8)) (mod (up-arrow 2 3 2) (expt 14 8))
	 (up-arrow-mod 2 2 4 (expt 14 8)) (up-arrow-mod 2 3 2 (expt 14 8))
	 (up-arrow-mod-bin 2 2 4 (expt 14 8)) (up-arrow-mod-bin 2 3 2 (expt 14 8)))
      (= 4 
	 (mod (up-arrow 2 2 4) 7) (mod (up-arrow 2 3 2) 7)
	 (up-arrow-mod 2 2 4 7) (up-arrow-mod 2 3 2 7)
	 (up-arrow-mod-bin 2 2 4 7) (up-arrow-mod-bin 2 3 2 7)))
  (is (= 804023037
	 (- (mod-expt 2 65536 (expt 14 8)) 3)
	 (mod (ack 4 2) (expt 14 8))
	 (mod (- (up-arrow 2 2 5) 3) (expt 14 8))
	 (- (up-arrow-mod 2 2 5 (expt 14 8)) 3) 
	 (- (up-arrow-mod-bin 2 2 5 (expt 14 8)) 3))))

;; up-arrow-mod and up-arrow-mod-bin produces the same results.
;; up-arrow-bin is more efficient

;; user> (time (hyper-mod 2 2 5 (expt 14 8)))
;; "Elapsed time: 98.69023 msecs"
;; 804023040
;; user> (time (hyper-mod-bin 2 2 5 (expt 14 8)))
;; "Elapsed time: 2.327671 msecs"
;; 804023040
;; user> (time (reduce #(mod-expt-bin %2 %1 (expt 14 8)) [2 2 2 2 2 2 2]))
;; "Elapsed time: 0.938946 msecs"
;; 982905344
;; user> (time (hyper-mod-bin 2 2 7 (expt 14 8)))
;; "Elapsed time: 1.255187 msecs"
;; 982905344


;; ( 1 + 3 + 7 + 61 + A(3,2265536-3) + A(4,A(5,4)) + A(5,A(6,5)) ) mod 14^8

(time (mod (+ 1 3 7 61 
	      (- (hyper-mod-bin 2 2 7 (expt 14 8)) 3) 
	      (- (hyper-mod-bin 2 3 8 (expt 14 8)) 3) 
	      (- (hyper-mod-bin 2 4 9 (expt 14 8)) 3)) (expt 14 8)))

;;"Elapsed time: 8.005489 msecs"

(time (+ 1 3 7 61 (mod
		   (+ (- (hyper-mod-bin 2 2 7 (expt 14 8)) 3) 
		      (- (hyper-mod-bin 2 3 8 (expt 14 8)) 3) 
		      (- (hyper-mod-bin 2 4 9 (expt 14 8)) 3)) (expt 14 8))))
;;1180723775

(defn f [n modulus] 
  (cond (< n 4) (ack n n)
	:else 
	(- (hyper-mod-bin 2 (- n 2) (+ n 3) modulus) 3))
  )

; (mod (reduce + (map #(f % (expt 14 8)) (range 0 7))) (expt 14 8))

;; (defn g [n] (* n n))
;; user> (mod (reduce + (map #(g %) (range 0 10))) 7)
;; 5
;; user> (mod (reduce + (map #(mod (g %) 7) (range 0 10))) 7)
;; 5


(defn ↑ [a b]
  (mod-expt-bin a b (expt 14 8))

(defn ↑↑ [a b]
  (reduce #(mod-expt-bin %2 %1 (expt 14 8)) (repeat b a)))

(defn ↑↑↑ [a b]
  (reduce #(↑↑ %2 %1) (repeat b a)))

(defn ↑↑↑↑ [a b]
  (reduce #(↑↑↑ %2 %1) (repeat b a)))


;; (time (- (↑↑ 2 7) 3 ))
;; (time (- (↑↑↑ 2 8) 3 ))
;; (time (- (↑↑↑↑ 2 9) 3 ))
;; this is the key
;;user> (mod-expt-bin 2 (↑↑ 2 7) (expt 14 7))
;;10436864
