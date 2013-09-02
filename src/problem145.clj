(ns problem145
  (:use 
   [clojure.test :only (deftest is)]
   [clojure.math.numeric-tower :only (expt)]
   [tools.numbers :only (digit-list integer)]))

;; Some positive integers n have the property that the sum [ n + reverse(n) ] consists entirely of odd (decimal) digits. For instance, 36 + 63 = 99 and 409 + 904 = 1313. We will call such numbers reversible; so 36, 63, 409, and 904 are reversible. Leading zeroes are not allowed in either n or reverse(n).

;; There are 120 reversible numbers below one-thousand.

;; How many reversible numbers are there below one-billion (10^9)?




(defn reversible-number? [number]
  (if (zero? (mod number 10)) false
      (let [n (digit-list number)
	    r (reverse n)
	    sum  (+ (integer n) (integer r))]
	(every? odd? (digit-list sum)))))

(deftest test-reversible-number
  (is (= 120
	 (count (filter reversible-number? (range 1 1000)))))
  (is (and (reversible-number? 63) (reversible-number? 36)
	   (reversible-number? 409) (reversible-number? 904))))

;; very inefficient. as both parts of a reversible number is calculated
;; ..
;; there must be a system
;;

;;                    dsums    sums
;;        0        10    0       0       1 digit, none are reversible as all sums are even
;;       10       100    4      20    20 
;;      100      1000   20     100   120
;;     1000     10000   20     600   720
;;    10000    100000    0       0
;;   100000   1000000  100   18000 18720
;;  1000000   2000000    0       0
;;  2000000   3000000  100    2500
;;  3000000   4000000  100    2500
;;  1000000  10000000  400   50000
;; 12000000  13000000  400   14400
;; 10000000 100000000  500  540000     
;; (+ 540000 50000 18000 0 600 100 20)

(defn reversible-number-sums [number]
  (if (zero? (mod number 10)) 0
      (let [n (digit-list number)
	    r (reverse n)
	    sum  (+ (integer n) (integer r))]
	(if (every? odd? (digit-list sum)) sum 0)
	)))

;; (def f (sort (filter #(< 0 %) (map  #(reversible-number-sums %) (range 2000000 3000000)))))
(defn part [start end] 
  (sort (filter #(< 0 %) (map  #(reversible-number-sums %) (range start end)))))
(defn stats [f] {:distinct (count (distinct f)) :count (count f)})
;(def f3to4mill (part 3000000 4000000))

;; (def f4 (part 10000000 11000000))
;; p145> (stats f4)
;; {:distinct 500, :count 18000}

;;(def f9 (part 18000000 19000000))
;; p145> (stats f9)
;; {:distinct 100, :count 3600}
;(def f10 (sort (filter #(< 0 %) (map  #(reversible-number-sums %) (range 19000000 100000000)))))


;(def p9mill  (part 9000000  10000000))
;(def p9bill (part  19000000 20000000))

;; p145> (time (def p9bill (part  19000000 20000000)))
;; "Elapsed time: 104014.632655 msecs"
;; #'p145/p9bill
;; p145> (stats p9bill)
;; {:distinct 100, :count 3600}
;; (def f12to13mil (part 12000000 13000000))
;; (stats f12to13mil)
;; {:distinct 400, :count 14400}
;; (def f22to23mil (part 22000000 23000000))
;; (stats f22to23mil)
;; {:distinct 400, :count 14400}

(def f1000-2000 (part 1000 2000))
(def f2000-3000 (part 2000 3000))
(def f3000-4000 (part 3000 4000))
(def f4000-5000 (part 4000 5000))
(def f5000-6000 (part 5000 6000))
(def f6000-7000 (part 6000 7000))
(def f7000-8000 (part 7000 8000))
(def f8000-9000 (part 8000 9000))
(def f9000-10000 (part 9000 10000))

{ :f1000-2000 (stats f1000-2000 )
  :f2000-3000 (stats f2000-3000 )
  :f3000-4000 (stats f3000-4000 )
  :f4000-5000 (stats f4000-5000 )
  :f5000-6000 (stats f5000-6000 )
  :f6000-7000 (stats f6000-7000 )
  :f7000-8000 (stats f7000-8000 )
  :f8000-9000 (stats f8000-9000 )
  :f9000-10000(stats f9000-10000)}
{
 :f1000-2000 {:distinct 20, :count 120}, 
 :f2000-3000 {:distinct 20, :count 120}
 :f3000-4000 {:distinct 15, :count 90}, 
 :f4000-5000 {:distinct 15, :count 90}, 
 :f5000-6000 {:distinct 10, :count 60}, 
 :f6000-7000 {:distinct 10, :count 60}, 
 :f7000-8000 {:distinct 5, :count 30}, 
 :f8000-9000 {:distinct 5, :count 30}, 
 :f9000-10000 {:distinct 0, :count 0}, 
 }

;; p145> (time (def f (part  1000000 10000000)))
;; "Elapsed time: 1269788.252011 msecs"
;; #'p145/f
;; p145> (stats f)
;; {:distinct 400, :count 50000}
;; p145> (time (def f (part  10000000 100000000)))
;; "Elapsed time: 1.4706084966347E7 msecs"
;; #'p145/f
;; p145> (stats f)
;; {:distinct 500, :count 540000}

;; brute force galore
;; user> (+ 540000 50000 18000 0 600 100 20)
;; 608720
(defn problem145 [] 0)

(def explanation
     "a) for n = 2k for some integer k, none of the digit-additions can carry, so the outside pair of digits can be any one of 20 choices, and each of the inside pairs can be any one of thirty choices, for a total of 20*30^(k-1) n-digit reversible numbers 

b) for n = 4k + 1 for some integer k, there are no solutions -- the middle digit would be required to add with itself to produce an odd result. 

c) for n = 4k + 3 for some integer k, the outside pair must have an odd sum >10, so there are again 20 choices; the next-to-outer pair must have an even sum without carry (25 choices), the next-inner pair must again have odd sum >10, the next an even sum, etc., to the middle digit which must not carry when added to itself (5 choices), for a total of 100*500^k n-digit reversible numbers. 

There are then 0 + 20*30^0 + 100*500^0 + 20*30^1 + 0 + 20*30^2 + 100*500^1 + 20*30^3 + 0 = 20 + 100 + 600 + 18000 + 50000 + 540000 = 608720 reversible numbers < 10^9.")
