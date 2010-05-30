(ns problem211
  (meta {:description "For a positive integer n, let σ2(n) be the sum of the squares of its divisors. For example,

σ2(10) = 1 + 4 + 25 + 100 = 130.
Find the sum of all n, 0 < n < 64,000,000 such that σ2(n) is a perfect square.
"})
  (:use clojure.contrib.repl-utils)
  (:use	[tools.primes :only (divisors divisors#)]
	[tools.java-wrappers]
	[clojure.contrib.math :only (expt exact-integer-sqrt)]
	[clojure.test]))

;; to handle 64 million longs, we have to modify the memory requirements
;; (custom-set-variables '(swank-clojure-extra-vm-args '("-server" "-Xmx768M")))

;;(- (expt 2 31 ) 1 )

;; (def highest (divisors 64000000))
;; (type  (reduce + (map #(* % %) highest)))
;; java.lang.Long

;; probably a good idea to use a long-array
;; 5688888803185771
;; (exact-integer-sqrt 5688888803185771)
;; [75424722 114408487]
;; perfect squares can be in a int-array

(defn make-square-divs-transient [limit]
  (loop [i 0 d 2 divs (transient (vec (repeat limit 1)))]
    (if (= d limit) 
      (persistent! divs)
      (if (>= i limit) 
	(recur 0 (inc d) divs)
	(recur (+ i d) d (assoc! divs i (+ (divs i) (* d d))))))))

;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-transient 1000000))))
;; "Elapsed time: 16447.10586 msecs"
;; 7857957051233
;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-transient 2000000))))
;; "Elapsed time: 92184.425185 msecs"
;; 21133820489733

(defn make-square-divs-seq [limit]
  (loop [i 0, d 2, divs (long-array limit 1)]
    (if (= d limit) 
      divs
      (if (>= i limit) 
	(recur 0 (+ d 1) divs)
	(recur (+ i d) d (do (aset divs i (+ (aget divs i) (* d d))) divs))))))

;  (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-seq 100000)))
;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-seq 1000000))))
;; "Elapsed time: 10090.140854 msecs"
;; 7857957051233
;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-seq 2000000))))
;; "Elapsed time: 21896.321766 msecs"
;; 21133820489733
;; problem211> (time (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) (make-square-divs-seq 3000000))))
;; "Elapsed time: 33802.737233 msecs"
;; 61677816054533

;; problem211> (time (def d (make-square-divs-seq 1000000)))
;; "Elapsed time: 7112.918091 msecs"
;; #'problem211/d
;; problem211> (time (reduce + (filter #(zero? (second (exact-integer-sqrt %))) d)))
;; "Elapsed time: 3140.152284 msecs"
;; 7857957051233
(defn perfect-square-indexes [array]
  (filter #(zero? (second (exact-integer-sqrt (second %)))) (map #(list %1 %2) (iterate inc 0) array))
  )
(comment
  (def d (make-square-divs-seq 1000000))
  (def pfi (perfect-square-indexes d))
  ;(def cd (map #(count (divisors (second %))) pfi))
  (count pfi) ; 41
  (def d (make-square-divs-seq 2000000))
  (count pfi) ; 48  
  (def cd (map #(count (divisors (second %))) pfi))
  ;; 3000000 -> 53
  ;; 4000000 --> argument type mismatch => d^2 is larger than long
  ;; make-square-divs-transient can handle this, but is slower
  ;; 4000000 --> 58
  (def pfi4000000 '((1 1) (42 2500) (246 84100) (287 84100) (728 722500) (1434 2856100) (1673 2856100) (1880 4884100) (4264 24304900) (6237 45024100) (9799 96079204) (9855 113635600) (18330 488410000) (21352 607622500) (21385 488410000) (24856 825412900) (36531 1514610724) (39990 2313610000) (46655 2313610000) (57270 4747210000) (66815 4747210000) (92664 13011964900) (125255 16430112400) (156570 35532250000) (182665 35532250000) (208182 60762250000) (212949 51437332804) (242879 60762250000) (273265 77829840400) (380511 163426147600) (391345 159696144400) (411558 240198010000) (539560 410752810000) (627215 410752810000) (693160 668633290000) (730145 557979120400) (741096 821017210000) (773224 796252828900) (814463 668633290000) (931722 1219036810000) (992680 1371943690000) (1069895 1195304890000) (1087009 1219036810000) (1143477 1475034540100) (1166399 1371943690000) (1422577 2044042090000) (1592935 2643160608400) (1815073 3327340810000) (2281255 5423402592400) (2544697 6498930490000) (2713880 10268820250000) (2722005 8796088272400) (2828385 9556753960000) (3054232 12389344022500) (3132935 10268820250000) (3145240 13949478010000) (3188809 10268820250000) (3508456 16715832250000)))

;problem211> (map #(factors (first %)) (rest pfi4000000))
  (def pfi4000000factors '([2 3 7] [2 3 41] [7 41] [2 2 2 7 13] [2 3 239] [7 239] [2 2 2 5 47] [2 2 2 13 41] [3 3 3 3 7 11] [41 239] [3 3 3 5 73] [2 3 5 13 47] [2 2 2 17 157] [5 7 13 47] [2 2 2 13 239] [3 3 3 3 11 41] [2 3 5 31 43] [5 7 31 43] [2 3 5 23 83] [5 7 23 83] [2 2 2 3 3 3 3 11 13] [5 13 41 47] [2 3 5 17 307] [5 7 17 307] [2 3 13 17 157] [3 3 3 3 11 239] [7 13 17 157] [5 31 41 43] [3 3 3 17 829] [5 23 41 83] [2 3 7 41 239] [2 2 2 5 7 41 47] [5 17 47 157] [2 2 2 5 13 31 43] [5 13 47 239] [2 2 2 3 3 3 47 73] [2 2 2 19 5087] [13 31 43 47] [2 3 11 19 743] [2 2 2 5 13 23 83] [5 17 41 307] [7 11 19 743] [3 3 3 3 19 743] [13 23 47 83] [13 17 41 157] [5 31 43 239] [13 17 43 191] [5 23 83 239] [23 31 43 83] [2 2 2 5 13 17 307] [3 3 3 3 5 11 13 47] [3 3 3 5 7 41 73] [2 2 2 293 1303] [5 13 157 307] [2 2 2 5 7 47 239] [13 17 47 307] [2 2 2 7 31 43 47]))


  ;; 5000000 --> OutOfMemoryError
)

(comment
  (defn perfect-square? [d]
    (zero? (second (exact-integer-sqrt d))))
  (def perfect-square? (memoize perfect-square?))
)

(set! *warn-on-reflection* true)

(take 12 (map (fn [s] (reduce + (map #(* % %) s))) (map #(divisors %) (iterate inc 1))))

;; memory efficient, but makes too many calculations (finding divisors is expensive.. ) 
;; furthermore, multiplications are done a lot of times on the same numbers (also done with the other methods)
;; exact-integer-sqrt is not cheap either .. 
(defn reduce-sum [limit]
  (reduce +  (filter #(zero? (second (exact-integer-sqrt %))) 
		     (take limit (map (fn [s] (reduce + (map #(* % %) s))) (map #(divisors %) (iterate inc 1)))))))

