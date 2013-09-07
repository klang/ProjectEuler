(ns problem122
  (:use [clojure.test :only (deftest is)]
        [tools.primes :only (prime? prime-factors)]))

;       (range n² .. n)
; 1 ->  2
; 2 ->  4  3
; 3 ->  6  5  4
; 4 ->  8  7  6  5
; 5 -> 10  9  8  7  6  
; 6 -> 12 11 10  9  8  7
; 7 -> 14 13 12 11 10  9  8 
; 8 -> 16 .. 7
; 9 -> 18 .. 8

;;                           1
;;                           2 
;;                       3       4
;;                   6 5 4     8 7 6 5   
;;      12 11 10 9 8 7



;; ................1
;; ................|
;; ................2
;; ............./......\
;; ............/........\
;; ...........3..........4
;; ......../.....\.........\
;; ....../........\..........\
;; .....5..........6..........8
;; .../...\........|......./.....\
;; ..7....10......12......9.......16
;; ./..../..\..../..\..../.\...../..\
;; 14...11..20..15..24..13..17..18..32


;; http://en.wikipedia.org/wiki/Addition_chain
;; A003313 

;; 1 + 1 = 2 + 1 = 3 + 3 = 6 + 6 = 12 + 3 = 15 
;; 1 + 1 = 2 + 2 = 4 + 4 = 8 + 4 = 12 + 2 = 14 + 1 = 15
(defn num-1-in-binary-repr [n]
 "number-of-one-bits-in-binary-representation"
  (count (filter #(= \1 %) (seq (Integer/toBinaryString n)))))
(defn l [n]
  (+ (/ (Math/log n) (Math/log 2)) (- (num-1-in-binary-repr n) 1)) )

;l(1) = 0; if n is prime l(n)=l(n-1)+1;
;else {n=j*k j,k>1; l(k*j)=l(k)+l(j)},

(defn l [n]
  "factor-method" 
  (cond (zero? n) 0
	(= n 1) 1
	(prime? n) (+ (l (- n 1)) 1)
	:else (let [f (prime-factors n)
		    j (first f)
		    k (reduce * (rest f))
		    ] (+ (l k) (l j)))))

(defn m [k]
  "minimum number of multiplications to compute n^k (shortest addition path)"
  0)

(deftest test-m
  (is (= 5 (m 15))))

(defn problem122 [] (reduce + (map #(m %) (range 1 201))))

;; results from 
;; http://www.research.att.com/~njas/sequences/a003313.txt
(def l-shortest [1 2 2 3 3 4 3 4 4 5 4 5 5 5 4 5 5 6 5 6 6 6 5 6 6 6 6 7 6 7 5 6 6 7 6 7 7 7 6 7 7 7 7 7 7 8 6 7 7 7 7 8 7 8 7 8 8 8 7 8 8 8 6 7 7 8 7 8 8 9 7 8 8 8 8 8 8 9 7 8 8 8 8 8 8 9 8 9 8 9 8 9 9 9 7 8 8 8 8 9 8 9 8 9 9 9 8 9 9 9 8 9 9 9 9 9 9 9 8 9 9 9 9 9 9 10 7 8 8 9 8 9 9 9 8 9 9 10 9 10 10 10 8 9 9 9 9 9 9 10 9 9 9 10 9 10 10 10 8 9 9 9 9 9 9 10 9 10 9 10 9 10 10 10 9 10 10 10 9 10 10 10 9 10 10 10 10 10 10 11 8 9 9 9 9 10 9 10 9])

(def l-binary [1 2 2 3 3 4 3 4 4 5 4 5 5 6 4 5 5 6 5 6 6 7 5 6 6 7 6 7 7 8 5 6 6 7 6 7 7 8 6 7 7 8 7 8 8 9 6 7 7 8 7 8 8 9 7 8 8 9 8 9 9 10 6 7 7 8 7 8 8 9 7 8 8 9 8 9 9 10 7 8 8 9 8 9 9 10 8 9 9 10 9 10 10 11 7 8 8 9 8 9 9 10 8 9 9 10 9 10 10 11 8 9 9 10 9 10 10 11 9 10 10 11 10 11 11 12 7 8 8 9 8 9 9 10 8 9 9 10 9 10 10 11 8 9 9 10 9 10 10 11 9 10 10 11 10 11 11 12 8 9 9 10 9 10 10 11 9 10 10 11 10 11 11 12 9 10 10 11 10 11 11 12 10 11 11 12 11 12 12 13 8 9 9 10 9 10 10 11 9])

(def l-factor [1 2 2 3 3 4 3 4 4 5 4 5 5 5 4 5 5 6 5 6 6 7 5 6 6 6 6 7 6 7 5 7 6 7 6 7 7 7 6 7 7 8 7 7 8 9 6 8 7 7 7 8 7 8 7 8 8 9 7 8 8 8 6 8 8 9 7 9 8 9 7 8 8 8 8 9 8 9 7 8 8 9 8 8 9 9 8 9 8 9 9 9 10 9 7 8 9 9 8 9 8 9 8 9 9 10 8 9 9 9 8 9 9 10 9 9 10 9 8 10 9 9 9 9 9 10 7 10 9 10 9 10 10 9 8 9 10 11 9 11 10 10 8 10 9 10 9 10 9 10 9 9 10 10 9 10 10 10 8 11 9 10 9 10 10 11 9 10 9 10 10 11 10 10 9 11 10 11 9 10 10 10 10 10 10 10 11 10 10 11 8 9 9 10 10 11 10 11 9])

(def l-power-tree [1 2 2 3 3 4 3 4 4 5 4 5 5 5 4 5 5 6 5 6 6 6 5 6 6 6 6 7 6 7 5 6 6 7 6 7 7 7 6 7 7 7 7 7 7 8 6 7 7 7 7 8 7 8 7 8 8 8 7 8 8 8 6 7 7 8 7 8 8 9 7 8 8 8 8 9 8 9 7 8 8 8 8 8 8 9 8 9 8 9 8 9 9 9 7 8 8 8 8 9 8 9 8 9 9 9 8 9 9 9 8 9 9 9 9 9 9 9 8 9 9 9 9 9 9 10 7 8 8 9 8 9 9 9 8 9 9 10 9 10 10 10 8 9 9 9 9 9 9 10 9 9 10 10 9 10 10 10 8 9 9 9 9 9 9 10 9 10 9 10 9 10 10 10 9 10 10 10 9 10 10 10 9 10 10 10 10 10 10 11 8 9 9 9 9 10 9 10 9])

;(map #(count %) [l-shortest l-binary l-factor l-power-tree])
;(map #(reduce + %) [l-shortest l-binary l-factor l-power-tree])
; (1582 1688 1635 1584)
; 1583 not it
; 1582 correct .. 1 not counted then

;; python:
;; lim = 200
;;  
;; best = [None, [set([1])]]
;; for exp in range(2, lim + 1):
;;     facts = []
;;     for f1 in xrange(1, exp / 2 + 1):
;;         for constr in best[exp - f1]:
;;             if f1 in constr: facts.append(constr.union([exp]))
;;     bestlen = min(len(fact) for fact in facts)
;;     best.append([fact for fact in facts if len(fact) == bestlen])
;;  
;; print sum(len(b[0]) - 1 for b in best[1:])
