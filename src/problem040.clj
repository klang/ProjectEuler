(ns problem040
  (meta {:description "An irrational decimal fraction is created by concatenating the positive integers:

0.123456789101112131415161718192021...
             ^
             12th
It can be seen that the 12th digit of the fractional part is 1.

If d(n) represents the nth digit of the fractional part, find the value of the following expression."})
  (:use [tools.numbers :only (digits)]
        [clojure.math.numeric-tower :only (expt)]))

; d(1) x d(10) x d(100) x d(1000) x d(10000) x d(100000) x d(1000000) 


; 9     x 1 char  -      9 -     d(1) -> d(9)
; 90    x 2 chars -    180 -    d(10) -> d(189)
; 900   x 3 chars -   2700 -   d(190) -> d(2889)
; 9000  x 4 chars -  36000 -  d(2890) -> d(38889)
; 90000 x 5 chars - 450000 - d(38890) -> d(488889)

(defn d-limit [number]
  (let [max   (- (count (digits (- number 1))) 1)
	digits-in-term (reduce + (take max (map #(* (expt 10 (- % 1)) (* 9 %)) (iterate inc 1))))
	count-to       (reduce + (take max (map #(* (expt 10 (- % 1)) 9) (iterate inc 1))))
	qqq            (quot (- number digits-in-term) (+ 1 max))
;	items-to-take  (+ count-to qqq)
;	next-item      (rem (- number digits-in-term) (+ 1 max))
	easy-items-to-take (+ 1 (+ count-to qqq))
	]
    easy-items-to-take))

(.length (apply str (take (d-limit 1000000) (iterate inc 1))))

;; this takes up max-number of bytes of memory, which is ok for the 
;; values we want to calculate. 
;; If (d (expt 10 17)) was asked for another approach would be needed
(defn irrational-decimal [max-number]
     (apply str (take (d-limit max-number) (iterate inc 1))))

(defn d [n] 
  (if (= 1 n) 1
      (. Integer parseInt (str (nth (irrational-decimal n) (- n 1))) 10)))

(defn problem040 []
  (reduce * (map #(d %) [1 10 100 1000 10000 100000 1000000])))

;; instead of allocating space for the string each time, we could have allocated the largest string of interest and just found the other terms that way.. 
