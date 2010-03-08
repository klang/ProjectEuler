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

(defn f2 []
  (map first (iterate 
		(fn [[c n dn nn dnn]] [
				       (if (= dn dnn) (+ c 1) c)
				       nn
				       dnn
				       (+ nn 1)
				       (divisors# (+ nn 1))
				       ]) [0 2 2 3 2])))

