;; For non-negative integers m, n, the Ackermann function A(m, n) is defined as follows:


;; For example A(1, 0) = 2, A(2, 2) = 7 and A(3, 4) = 125.

;;Find 0 n  6 A(n, n) and give your answer mod 148.
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

(deftest test-ack
  (is (= 3 (ack 1 1)))   (is (= 4 (ack 1 2)))   (is (= 5 (ack 1 3)))    (is (= 6 (ack 1 4))) 
  (is (= 7 (ack 1 5)))   (is (= 8 (ack 1 6)))   (is (= 9 (ack 1 7)))    (is (= 10 (ack 1 8)))
  (is (= 5 (ack 2 1)))   (is (= 7 (ack 2 2)))   (is (= 9 (ack 2 3)))    (is (= 11 (ack 2 4)))
  (is (= 13 (ack 2 5)))  (is (= 15 (ack 2 6)))  (is (= 17 (ack 2 7)))   (is (= 19 (ack 2 8)))
  (is (= 13 (ack 3 1)))  (is (= 29 (ack 3 2)))  (is (= 61 (ack 3 3)))   (is (= 125 (ack 3 4)))
  (is (= 253 (ack 3 5))) (is (= 509 (ack 3 6))) (is (= 1021 (ack 3 7))) (is (= 2045 (ack 3 8)))
  (is (= 13 (ack 4 0)))  (is (= 65533 (ack 4 1))) (is (= 19729 (count (digits (ack 4 2)))))
)

(def ack (memoize ack))


(defn Amod [m n]
  (cond (= m 0) (mod (+ n 1) (expt 14 8))
	(and (< 0 m) (= n 0)) (Amod (- m 1) 1)
	(and (< 0 m) (< 0 n)) (Amod (- m 1) (Amod m (- n 1)))))

(defn mod-mult [a b m]
  "(mod (* a b) m) == (mod (* (mod a m) (mod b m)) m)"
  (mod (* (mod a m) (mod b m)) m))

;; http://en.wikipedia.org/wiki/Modular_exponentiation#Memory-efficient_method
(defn mod-expt [base exp m]
  (loop [e 1, c base]
    (if (= e exp) c (recur (inc e) (mod (* c base) m)))))

(deftest test-mod-expt
  (is (= 445 (mod-expt 4 13 497))))

(defn ackm [m n]
  (do (println [m n]))
  (cond (= m 0) (mod (+ n 1) 1475789056)
	(= m 1) (mod (+ n 2) 1475789056)
	(= m 2) (mod (+ 3 (* 2 n)) 1475789056)
;	(= m 3) (mod (+ 5 (* 8 (- (expt 2 n) 1))) 1475789056)
	(= m 3) (mod (+ 5 (* 8 (- (mod-expt 2 n 1475789056) 1))) 1475789056)
        (= n 0) (recur (- m 1) 1)
        :else (recur (- m 1) (ackm m (- n 1)))))

(def ackm (memoize ackm))

;; ( 1 + 3 + 7 + 61 + A(3,2265536-3) + A(4,A(5,4)) + A(5,A(6,5)) ) mod 14^8

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


(deftest test-hyper
  (= (- (expt 2 (expt 2 (expt 2 (expt 2 2)))) 3) 
     (- (expt 2 65536) 3)
     (ack 4 2))

  (= (- (mod-expt 2 (mod-expt 2 
			      (mod-expt 2 
					(mod-expt 2 2 
						  (expt 14 8)) 
					(expt 14 8)) (expt 14 8)) (expt 14 8)) 3)
     (- (mod-expt 2 65536 (expt 14 8)) 3)
     (mod (ack 4 2) (expt 14 8))))

;(mod-expt 2 65536 (expt 14 8))

(mod-expt 2 (mod-expt 2 (mod-expt 2 (mod-expt 2 2 (expt 14 8)) (expt 14 8)) (expt 14 8)) (expt 14 8))

(expt 2 (expt 2 (expt 2 (expt 2 2))))

(hyper 2 1 4)
; 2↑4 = 2 * 2 * 2 * 2 = (expt 2 4)
(hyper 2 2 4)
; 2↑↑4 = 2↑2↑2↑2↑2 = (expt 2 (expt 2 (expt 2 2)))
(expt 2 (expt 2 (expt 2 2)))
(hyper 2 3 2)
; 2↑↑↑8 = 2↑↑(2↑↑↑7) = 2↑↑(2↑↑(2↑↑↑6)) = 2↑↑(2↑↑(2↑↑(2↑↑↑5))) = 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑4)))) = 
; 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑3))))) = 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑2)))))) = 
; 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑1))))))) = 2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑(2↑↑↑1))))))) =

(defn hyper [a n b]
  (reduce #(expt %2 %1) (repeat (* (- n 1) b) a)))
;    (hyper 2 2 4 (expt 14 8))
; == (hyper 2 3 2 (expt 14 8))
; 65536

(defn hyper-mod [a n b m]
  (reduce #(mod-expt %2 %1 m) (repeat (* (- n 1) b) a)))
;    (hyper-mod 2 2 4 (expt 14 8))
; == (hyper-mod 2 3 2 (expt 14 8))
; 65536

(f 2 5)
(defn ↑↑ [a b]
  "a↑↑b"
  (println a "↑↑" b))

; http://en.wikipedia.org/wiki/Hyper_operator
(defn Hn [n a b]
  (cond (zero? n) (inc b)
	(and (= n 1) (zero? b)) a
	(and (= n 2) (zero? b)) 0
	(and (> n 2) (zero? b)) 1
    :else (recur (dec n)
		 a
		 (Hn n a (dec b)))))

(load "problem188")

(defn hyper-mod-bin [a n b m]
  (reduce #(mod-expt-bin %2 %1 m) (repeat (* (- n 1) b) a)))

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

(time (mod (+ 1 3 7 61 (- (hyper-mod-bin 2 2 7 (expt 14 8)) 3) (- (hyper-mod-bin 2 3 8 (expt 14 8)) 3) (- (hyper-mod-bin 2 4 9 (expt 14 8)) 3)) (expt 14 8)))
;;"Elapsed time: 8.005489 msecs"
;;1180723775
(time (+ 1 3 7 61 (mod
		   (+ (- (hyper-mod-bin 2 2 7 (expt 14 8)) 3) 
		      (- (hyper-mod-bin 2 3 8 (expt 14 8)) 3) 
		      (- (hyper-mod-bin 2 4 9 (expt 14 8)) 3)) (expt 14 8))))

(defn f [n modulus] 
  (cond (< n 4) (ack n n)
	:else 
	(- (hyper-mod-bin 2 (- n 2) (+ n 3) modulus) 3))
  )
; (mod (+ (f 0) (f 1) (f 2) (f 3) (f 4) (f 5) (f 6)) (expt 14 8))
;==
(+ (mod (f 0) (expt 14 8)) (mod (f 1) (expt 14 8)) (mod (f 2) (expt 14 8)) (mod (f 3) (expt 14 8)))

; (mod (reduce + (map #(f % (expt 14 8)) (range 0 7))) (expt 14 8))
