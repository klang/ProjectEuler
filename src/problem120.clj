(ns problem120
  (:use clojure.contrib.math)
  (:use clojure.test))

;; Let r be the remainder when (a-1)^n + (a+1)^n is divided by a².

;; For example, if a = 7 and n = 3, then r = 42: 63 + 83 = 728  42 mod 49. 
;; And as n varies, so too will r, but for a = 7 it turns out that r-max = 42.

;; For 3 <= a <= 1000, find sum r_max.

(defn euler [a n]
  (rem (+ (expt (- a 1) n) (expt (+ a 1) n)) (* a a)))

(defn maximum [n]
  (last (sort (distinct (map #(euler n %) (range 1 (* 2 n)))))))

;; problem120> (maximum 7)
;; 42

;; problem120> (time (reduce + (map maximum (range 3 300))))
;; "Elapsed time: 10308.482343 msecs"
;; 8887850
;; problem120> (time (reduce + (map maximum (range 300 350))))
;; "Elapsed time: 8980.675778 msecs"
;; 5251100
;; problem120> (time (reduce + (map maximum (range 340 400))))
;; "Elapsed time: 14626.873735 msecs"
;; 8176570

;; a little faster
(defn maximum [n]
  (reduce #(max %1 %2) (map #(euler n %) (range 1 (* 2 n)))))

;; problem120> (time (reduce + (map maximum (range 400 450))))
;; "Elapsed time: 18746.160445 msecs"
;; 8988600
;; problem120> (time (reduce + (map maximum (range 450 500))))
;; "Elapsed time: 27474.452925 msecs"
;; 11232350

;; faster still .. as we notice that euler = 2 for even values
(defn maximum [n]
  (reduce #(max %1 %2) (map #(euler n %) (range 1 (* 2 n) 2))))

;; problem120> (time (reduce + (map maximum (range 500 600))))
;; "Elapsed time: 43799.653571 msecs"
;; 30195950
;; problem120> (time (reduce + (map maximum (range 700 750))))
;; "Elapsed time: 51037.604835 msecs"
;; 26201100
;; problem120> (time (reduce + (map maximum (range 750 800))))
;; "Elapsed time: 62652.631227 msecs"
;; 29944850
;; problem120> (time (reduce + (map maximum (range 800 900))))
;; "Elapsed time: 174200.663861 msecs"
;; 72120950
;; problem120> (time (reduce + (map maximum (range 900 1001))))
;; "Elapsed time: 251907.554752 msecs"
;; 91093950

;; (+ 8887850 5251100 8176570 8988600 11232350 30195950 26201100 29944850 72120950 91093950)
;; 292093270 <-- incorrect
