(ns problem005
  (meta {:description "What is the smallest number divisible by each of the numbers 1 to 20?"})
  (:use [clojure.contrib.math :only (lcm)]))



;; every number in the range is checked: very inefficient
(defn divides-all? [n m]
  (= 0 (reduce + (map #(mod n %) (range 2 (+ 1 m))))))

;; bailing out when reaching a number that does not divide: better
(defn divides-all? [n m]
  (= m (count (take-while #(zero? (mod n %)) (range 1 (+ 1 m))))))

; user> (time (problem005 8))
; "Elapsed time: 8.996676 msecs"
; 840
; user> (time (problem005 9))
; "Elapsed time: 248.228011 msecs"
; 2520
; user> (time (problem005 10))
; "Elapsed time: 33.915778 msecs"
; 2520
; user> (time (problem005 11))
; "Elapsed time: 307.529708 msecs"
; 27720
; user> (time (problem005 12))
; "Elapsed time: 312.252092 msecs"
; 27720
; user> (time (problem005 13))
; "Elapsed time: 4018.973998 msecs"
; 360360
; user> (time (problem005 15))
; "Elapsed time: 4520.597666 msecs"
; 360360
; user> (time (problem005 16))
; "Elapsed time: 9026.046415 msecs"
; 720720; user> (time (problem005 17))
; "Elapsed time: 161307.22162 msecs"
; 12252240
; 

(defn- divides-all? [n m]
  (= (- m 1) (count (take-while #(true? %) (map #(zero? (mod n %)) (range 1 m))))))


(defn- problem005 [m]
  (loop [m m
	 s 1]
    (if (divides-all? s m)
      s
      (recur m (inc s)))))

;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
(use 'clojure.contrib.test-is)

(deftest test-problem005
  (is (= 2520 (problem005 10))))

; (run-tests)

(comment
  (reduce max (for [x1 (range 1 (+ 10 1))
		    x2 (range 2 (+ 10 2))
		    x3 (range 3 (+ 10 3))]
		(if (= x1 x2 x3) x1 0)
		)))


(defn- re10 []
  (loop [n 2]
    (if (and  (zero? (rem n 1)) (zero? (rem n 2)) (zero? (rem n 3))
	      (zero? (rem n 4)) (zero? (rem n 5)) (zero? (rem n 6))
	      (zero? (rem n 7)) (zero? (rem n 8)) (zero? (rem n 9))
	      (zero? (rem n 10))) 
      n
      (recur (+ n 2)))))

;; starting from 1 .. (inc 1)
;;user> (time (re10))
;;"Elapsed time: 6.057196 msecs"
;;2520

;; starting from 2 .. (+ n 2)
;;user> (time (re10))
;;"Elapsed time: 2.617652 msecs"
;;2520

;; checking every number (also the odd ones, that will not be possible results)
(defn- re20all []
  (loop [n 1]
    (if (and  (zero? (rem n 2)) (zero? (rem n 3)) (zero? (rem n 4)) 
	      (zero? (rem n 5)) (zero? (rem n 6)) (zero? (rem n 7)) 
	      (zero? (rem n 8)) (zero? (rem n 9)) (zero? (rem n 10))
	      (zero? (rem n 11))
	      (zero? (rem n 12)) (zero? (rem n 13)) (zero? (rem n 14)) 
	      (zero? (rem n 15)) (zero? (rem n 16)) (zero? (rem n 17)) 
	      (zero? (rem n 18)) (zero? (rem n 19)) (zero? (rem n 20))

) 
      n
      (recur (inc n)))))

;; user> (time (re20all))
;; "Elapsed time: 149766.979853 msecs"
;; 232792560

(defn- re20even []
  (loop [n 2]
    (if (and  (zero? (rem n 2)) (zero? (rem n 3)) (zero? (rem n 4)) 
	      (zero? (rem n 5)) (zero? (rem n 6)) (zero? (rem n 7)) 
	      (zero? (rem n 8)) (zero? (rem n 9)) (zero? (rem n 10))
	      (zero? (rem n 11))
	      (zero? (rem n 12)) (zero? (rem n 13)) (zero? (rem n 14)) 
	      (zero? (rem n 15)) (zero? (rem n 16)) (zero? (rem n 17)) 
	      (zero? (rem n 18)) (zero? (rem n 19)) (zero? (rem n 20))

) 
      n
      (recur (+ n 2)))))

;; user> (time (re20even))
;; "Elapsed time: 105269.64332 msecs"
;; 232792560

(defn- re20even2 []
  (loop [n 2]
    (if (and  (= 0 (rem n 3)) (= 0 (rem n 4)) 
	      (= 0 (rem n 5)) (= 0 (rem n 6)) (= 0 (rem n 7)) 
	      (= 0 (rem n 8)) (= 0 (rem n 9)) (= 0 (rem n 10))
	      (= 0 (rem n 11))
	      (= 0 (rem n 12)) (= 0 (rem n 13)) (= 0 (rem n 14)) 
	      (= 0 (rem n 15)) (= 0 (rem n 16)) (= 0 (rem n 17)) 
	      (= 0 (rem n 18)) (= 0 (rem n 19)) (= 0 (rem n 20))

) 
      n
      (recur (+ n 2)))))

;; user> (time (re20even2))
;; "Elapsed time: 77198.058512 msecs"
;; 232792560

(defn- re20even2mod []
  (loop [n 2]
    (if (and  (= 0 (mod n 3)) (= 0 (mod n 4)) 
	      (= 0 (mod n 5)) (= 0 (mod n 6)) (= 0 (mod n 7)) 
	      (= 0 (mod n 8)) (= 0 (mod n 9)) (= 0 (mod n 10))
	      (= 0 (mod n 11))
	      (= 0 (mod n 12)) (= 0 (mod n 13)) (= 0 (mod n 14)) 
	      (= 0 (mod n 15)) (= 0 (mod n 16)) (= 0 (mod n 17)) 
	      (= 0 (mod n 18)) (= 0 (mod n 19)) (= 0 (mod n 20))) 
      n
      (recur (+ n 2)))))

;; finally checking if mod is faster than rem
;; user> (time (re20even2mod))
;; "Elapsed time: 126468.241237 msecs"
;; 232792560

(defn- re20even2flip []
  (loop [n 2]
    (if (and  (= 0 (rem n 19)) (= 0 (rem n 17)) (= 0 (rem n 13))
	      (= 0 (rem n 11)) (= 0 (rem n 7)) (= 0 (rem n 5))
	      (= 0 (rem n 3))
	      (= 0 (rem n 18)) (= 0 (rem n 9))  
	      (= 0 (rem n 20))
	      (= 0 (rem n 15)) 
	      (= 0 (rem n 16)) 
	      (= 0 (rem n 12))(= 0 (rem n 6)) (= 0 (rem n 4)) 
	      (= 0 (rem n 8)) 
	      (= 0 (rem n 10))
	      (= 0 (rem n 14))
) 
      n
      (recur (+ n 2)))))
;; user> (time (re20even2flip))
;; "Elapsed time: 62725.00717 msecs"
;; 232792560

(defn- re20even2flip2 []
  (loop [n 2]
    (if (and  (= 0 (rem n 19)) (= 0 (rem n 17)) (= 0 (rem n 13))
	      (= 0 (rem n 11)) (= 0 (rem n 7)) (= 0 (rem n 5)) (= 0 (rem n 3))
	      (= 0 (rem n 10)) (= 0 (rem n 20))
	      (= 0 (rem n 12)) (= 0 (rem n 6)) (= 0 (rem n 4)) 
	      (= 0 (rem n 14))
	      (= 0 (rem n 15)) 
	      (= 0 (rem n 16)) (= 0 (rem n 8)) 
	      (= 0 (rem n 18)) (= 0 (rem n 9))  
	      ) 
      n
      (recur (+ n 2)))))
;; user> (time (re20even2flip2))
;; "Elapsed time: 62720.427546 msecs"
;; 232792560
(def problem005 re20even2flip2)

;; user> (time (reduce lcm (range 1 21)))
;; "Elapsed time: 0.900674 msecs"
;; 232792560

(defn problem005 [] (reduce lcm (range 1 21)))