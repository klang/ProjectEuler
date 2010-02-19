;; Consider the fraction, n/d, where n and d are positive integers. If nd and HCF(n,d)=1, it is called a reduced proper fraction.
;; 
;; If we list the set of reduced proper fractions for d <  8 in ascending order of size, we get:
;; 
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;; 
;; It can be seen that 2/5 is the fraction immediately to the left of 3/7.
;; 
;; By listing the set of reduced proper fractions for d  1,000,000 in ascending order of size, find the numerator of the fraction immediately to the left of 3/7.

;(distinct (for [d (range 1 8) n (range 1 d)] (/ n d)))

(load "tools")
(use 'clojure.contrib.math)
(load "problem026")

;; user> (long-divide 3 7)
;; "0.(428571)"
;; user> (/ 428571 999999)
;; 3/7
;; user> (long-divide 428571 999999)
;; "0.(428571)"
;; user> (long-divide 428571 1000000)
;; "0.428571"
;; (gcd 428571 1000000)
;; 1
;; user> (sort [(/ 428571 1000000) 3/7] )
;; (428571/1000000 3/7)

;; 0.428571          == 0.428571        == 428571/1000000  OK
;; 0.42857(1)        == 0.4285711111111 == 192857/450000   OK
;; 0.4(28571)28571   == 0.4285712857128 == 428567/999990   OK (gcd 428567 999990)
;; 0.(428571)428571  == 0.4285714285714 == 3/7 == 428571/999999
;; 0.428(571)571     == 0.4255715715716 == 428143/999000   X
;; 0.4285(71)71      == 0.4255717171717 == 212143/495000   X
;; 0.42(8571)8571    == 0.4285718571857 == 142843/333300   X
;;(sort [428571/1000000 192857/450000 428567/999990 3/7 428143/999000 212143/495000 142843/333300])

;; user> (- 3/7 428567/999990)
;; 1/6999930
;; user> (long-divide-cycle-length 1 6999930)
;; 30
;; user> (long-divide 1 6999930)
;; "0.0(000001428585714428572857157143)"
;; -- find-fraction does not support this type of repeating cycles .. 

;; getting closer to 3/7 must be a fraction between 1/6999930 and 1/7000000
;; where the determinant reduce to something below 1000000 ..
;(count (sort (map #(- 3/7 (/ 1 %)) (range 6999930 7000000))))
; ..copied to scratch and reduced to the following

(sort [428567/999990 428570/999997 3/7])

;; user> (- 3/7 428570/999997)
;; 1/6999979
;; at least that's closer than the previous candidate
;;user> (long-divide-cycle-length 1 6999979)
;;2970
;;user> (long-divide 1 6999979)
;;"0.(0000001428575714298571467142....
;;"0.0(000001428585714428572857157143)"


;; -- find-fraction does not support this type of repeating cycles .. 

(defn find-fraction [nrp rp]
  "finds a fraction n/p where (long-divide n p) returns 0.npr(rp).
That is, the non repeating part of the decimal form is followed by the specified repeating part
Repeating parts starting with zeros are not supported.
"
  (let [nrpc (count (digits nrp))
	rpc (count (digits rp))
	c (+ nrpc rpc)
	nines (take rpc (repeat 9))
	zeros (take nrpc (repeat 0))
	d2 (integer (into zeros nines))
	hcd (/ d2 (gcd (expt 10 nrpc) d2))
	n1 (* nrp hcd)
	d1 (* (expt 10 nrpc) hcd)
	n2 rp
	n3 (+ n1 n2)
	d3 d1]
    (/ n3 d3)))

;; there are simply too many calculations to do this by brute force this one.
;;(for [d 1000000 n (range 428571 d) :when (and (= 1 (gcd n d)) )] [n d])
;(filter #(<= % 3/7) (sort (for [d (range 1 101) n (range 1 d) :when (and (= 1 (gcd n d)) (< (/ n d) 3/7))] (/ n d))))

