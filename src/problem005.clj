;; What is the smallest number divisible by each of the numbers 1 to 20?

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

(defn divides-all? [n m]
  (let [a (take-while #(zero? (mod n %)) (range 1 (+ 1 m)))]
    ))

(defn divides-all? [n m]
  (= m (count (take-while #(= % true) 
			  (map #(= 0 (mod n %)) 
			       (range 1 (+ 1 m))))))
  )

(defn divides-all? [n m]
  (= (- m 1) (count (take-while #(true? %) (map #(zero? (mod n %)) (range 1 m)))))
  )

(defn divides-all? [n m]
  (let [r (sieve m)
	c (count r)]
    (= c (count (take-while #(= % true) (map #(= 0 (mod n %)) r)))))
  )

; new approach ..
(defn divides-all? [n m]
  (= m (count (take-while #(zero? (mod n %)) [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20]))))
(defn divides-all? [n m]
  (= m (count (take-while #(zero? (mod n %)) [1 2 3 4 5 6 7 8 9 10]))))
(defn divides-all? [n m]
  (= m (count (take-while #(zero? (mod n %)) [10]))))
(defn problem005 [m]
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



