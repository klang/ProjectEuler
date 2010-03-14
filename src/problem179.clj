;; Find the number of integers 1 < n < 10^7, for which n and n + 1 have the same number of positive divisors. 

;; For example, 14 has the positive divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.
(load "tools")

(defn f [limit]
  (loop [n 2, dn (divisors# n), c 0]
    (if (< limit n) 
      c
      (let [nn (inc n)
	    dnn (divisors# nn)
	    c   (if (= dn dnn) (inc c) c)]
;	(do (println {:n n :dn dn :nn nn :dnn dnn :c c}))
	(recur nn dnn c)))))

;; user> (time (f (expt 10 5)))
;; "Elapsed time: 78273.246317 msecs"
;; 10585

;; not very efficient
;; and runs out of memory !?!?
;; Java heap space
;;  [Thrown class java.lang.OutOfMemoryError]

(defn f1 [limit]
  (loop [n 2, dn 2, nn 3, dnn 2, c 0]
    (if (< limit n) 
      c
;      (do (println {:n n :dn dn :nn nn :dnn dnn :c c}))
      (recur nn dnn (+ nn 1) (divisors# (+ nn 1)) (if (= dn dnn) (+ c 1) c)))))

;;user> (time (f1 (expt 10 5)))
;;"Elapsed time: 74362.237537 msecs"
;;10585

;user> (time (f1 (expt 10 7)))
; Evaluation aborted.
;;Java heap space
;;  [Thrown class java.lang.OutOfMemoryError]

;; why am I holding onto the head?

(defn f2 [limit] (for [n (range 2 limit) :when (= (divisors# n) (divisors# (+ n 1)))] n))
;; user> (time (count (f2 (expt 10 4))))
;; "Elapsed time: 6969.99566 msecs"
;; 1119

(defn free-mem [] (.freeMemory (Runtime/getRuntime)))
(defn total-mem [] (.totalMemory (Runtime/getRuntime)))
(defn max-mem [] (.maxMemory (Runtime/getRuntime)))


(def pfs (map #(prime-factors %) (iterate inc 2)))
(def divs (map #(divisors# %) (iterate inc 2)))

(defn f3 [limit] 
  (loop [d divs, c 0 , i 0]
    (if (<  limit i)
      c
      (do (println {:c c :i i :d (first d)})
	  (recur (rest d) (if (= (first d) (second d)) (inc c) c) (inc i))))))


(defn f3 [limit] 
  (loop [d (map #(divisors# %) (iterate inc 2)), c 0 , i 0]
    (if (<  limit i)
      c
;      (do (println {:c c :i i :d (first d)}))
      (recur (rest d) (if (= (first d) (second d)) (inc c) c) (inc i)))))
;; user> (time (f3 10000))
;; "Elapsed time: 5636.747636 msecs"
;; 1119
;; user> (time (f3 100000))
;; "Elapsed time: 73683.396872 msecs"
;; 10585

;; what is special about this number?
;;user> (time (f3 131069))
;; Evaluation aborted.
;; user> (divisors# 131069)
;; 4
;; user> (divisors# 131070)
;; 32
;; user> (count (take 131070 divs))
;; 131070
;; user> (count (take 131071 divs))
;; ; Evaluation aborted.
;; user> (prime? 131071)
;; true

;; generating and remembering more than 131070 primes will take up the 64M default memory
;;(custom-set-variables '(swank-clojure-extra-vm-args '("-server" "-Xmx512M")) )

;; user> (time (f3 10000))
;; "Elapsed time: 8026.056974 msecs"
;; 1119
;; user> (time (f3 10000000))


(def f4
  (loop [d divs, c 0 , i (expt 10 2)]
    (if (<  0 i)
      c
      (recur (rest d) (if (= (first d) (second d)) (inc c) c) (dec i)))))


(defn f5 [start limit]
  (loop [n start, dn (divisors# n), c 0]
    (if (< limit n) 
      c
      (let [nn (inc n)
	    dnn (divisors# nn)
	    c   (if (= dn dnn) (inc c) c)]
	(do (println {:c c :i n :d dn})
	    (recur nn dnn c))))))

; (f5 2 1000000)
