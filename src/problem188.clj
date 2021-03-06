(ns problem188 
  (meta {:description "The hyperexponentiation or tetration of a number a by a positive integer b, denoted by a↑↑b, is recursively defined by:

a↑↑1 = a,
a↑↑(k+1) = a^(a↑↑k).

Thus we have e.g. 3↑↑2 = 3^3 = 27, hence 3↑↑3 = 3^27 = 7625597484987 and 3↑↑4 is roughly 10^3.6383346400240996*10^12.

Find the last 8 digits of 1777↑↑1855."})
  (:use [clojure.math.numeric-tower :only (expt)]))

; (mod (↑↑ 1777 1855) (expt 10 8))

; (mod (expt 1777 1855) (expt 10 8))
; 47576593
; user> (prime? 47576593)
; true

; user> (time (mod-expt 1777 1855 (expt 10 8)))
; "Elapsed time: 2.657601 msecs"
; 47576593

; 1777↑↑1855 = 1777↑(1777↑↑1854) = 1777↑(1777↑(1777↑↑1853)) =  1777↑1777↑ ..↑1777 
;                                                               \              /
;                                                                 --1855times--


;; http://en.wikipedia.org/wiki/Modular_exponentiation#Memory-efficient_method
(defn mod-expt [base exp m]
  (loop [e 1, c base]
    (if (= e exp) c (recur (inc e) (mod (* c base) m)))))

;; user> (time (mod-expt 1777 1777 (expt 10 8)))
;; "Elapsed time: 24.952622 msecs"
;; 87955697

;; user> (time (mod-expt 1777 87955697 (expt 10 8)))
;; "Elapsed time: 123353.921124 msecs"
;; 99034097

;; combine mod-expt 
;; with http://en.wikipedia.org/wiki/Binary_exponentiation 
;; as we do not have an optimal addition chain algorithm yet

;;http://en.wikipedia.org/wiki/Modular_exponentiation#Right-to-left_binary_method
(defn mod-expt-bin [base exponent modulus]
  (loop [result 1 base base
	 exponent exponent]
    (if (= 0 exponent)
      result
      (recur (if (odd? exponent)
	       (mod (* result base) modulus)
	       result)
	     (mod (* base base) modulus) 
	     (bit-shift-right exponent 1)))))


;; user> (time (mod-expt-bin 1777 1777 (expt 10 8)))
;; "Elapsed time: 0.501181 msecs"
;; 87955697
;; user> (time (mod-expt 1777 87955697 (expt 10 8)))
;; "Elapsed time: 0.338032 msecs"
;; 99034097
;; .. a slight improvement over the 123353.921124 msecs :-D

;; user> (time (mod-expt-bin 1777 1855 (expt 10 8)))
;; "Elapsed time: 1.283125 msecs"
;; 47576593
;; ..

;; 1777↑↑1855 = 1777↑(1777↑↑1854) = 1777↑(1777↑(1777↑↑1853)) =  1777↑1777↑ ..↑1777 
;;                                                               \              /
;;                                                                 --1855times--

(defn hyper-mod-bin [a n b m]
  (reduce #(mod-expt-bin %2 %1 m) (repeat (* (- n 1) b) a)))

;; user> (time (hyper-mod-bin 1777 2 1855 (expt 10 8)))
;; "Elapsed time: 235.245622 msecs"
;; 95962097

(defn exptm [base pow modulus]
  "Binary exponentation kept down"
  (.modPow (biginteger base) (biginteger pow) (biginteger modulus)))

(defn hyperm [a b m]
  "returns a↑↑b mod m, keeps the results low"
  ;(do (println (list a b m (repeat b a))))
  (reduce #(exptm %2 %1 m) (repeat b a)))

;; user> (time (hyperm 1777 1855 (expt 10 8)))
;; "Elapsed time: 208.373776 msecs"
;; 95962097

(defn problem188 [] (hyperm 1777 1855 (expt 10 8)))
