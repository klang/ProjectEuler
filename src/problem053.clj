; There are exactly ten ways of selecting three from five, 12345:

; 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345

; In combinatorics, we use the notation, C(5,3) = 10.

;In general,

; [formular snipped, but implemented below]

;It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.

(load "tools")

(def factorial (memoize factorial))

(defn C [n r]
  (assert (and (integer? n) (integer? r) (<= r n)))
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

(deftest test-p053
  (is (= 10 (C 5 3)))
  (is (= 1144066 (C 23 10))))

;How many, not necessarily distinct, values of  (C n r) , for 1 <= n <= 100, are greater than one-million?

(defn dv [limit]
  (map (fn [[n r]] (C n r)) (for [n (range 1 (+ limit 1))
				  r (range 1 n)] [n r])))

;(filter #(< 1000000 %) (dv 23))

;; user> (count (filter #(< 1000000 %) (dv 100)))
;; 4075
