(use 'clojure.contrib.math)

;; http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Example.2C_square_root_of_114_as_a_continued_fraction

;; this problem gives a method for calculating continued fractions for irrational numbers. problem 64 gives the method for rational numbers.

; multimethod (rational? number)   --> [a0 a1 a2 a3]  (cycle [a1 a2 a3])
; rational-numbers                 --> [a0 a1 a2 a3]

(defn contiuned-fraction-for-sqrts [S]
  "(contiuned-fraction-for-sqrts S) expects any ny natural number which is not a perfect square. returns [a0 a1 a2 a3 .. an] where (a1 .. an) is cyclic.
The calculation is based on http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion"
  (let [s (first (exact-integer-sqrt S))]
    (loop [m0 0 d0 1 a0 s
	   c [a0] seen #{} ]
      (let [m1 (- (* d0 a0) m0) 
	    d1 (/ (- S (* m1 m1)) d0)
	    a1 (floor (/ (+ s m1) d1))]
	(if (seen (list m1 d1 a1)) c
	  (recur m1 d1 a1 
		 (conj c a1) (conj seen (list m1 d1 a1))))))))

(defn odd-period? [continued-fraction]
  (even? (count continued-fraction)))

(defn even-period? [continued-fraction]
  (odd? (count continued-fraction)))

(defn period [continued-fraction]
  (- (count continued-fraction) 1))

(deftest test-continued-fraction-for-sqrts
  (is (= 1 (period (contiuned-fraction-for-sqrts 2))))
  (is (= 2 (period (contiuned-fraction-for-sqrts 3))))
  (is (= 1 (period (contiuned-fraction-for-sqrts 5))))
  (is (= 2 (period (contiuned-fraction-for-sqrts 6))))
  (is (= 4 (period (contiuned-fraction-for-sqrts 7))))
  (is (= 2 (period (contiuned-fraction-for-sqrts 8))))
  (is (= 1 (period (contiuned-fraction-for-sqrts 10))))
  (is (= 2 (period (contiuned-fraction-for-sqrts 11))))
  (is (= 2 (period (contiuned-fraction-for-sqrts 12))))
  (is (= 5 (period (contiuned-fraction-for-sqrts 13))))
  (is (= 6 (period (contiuned-fraction-for-sqrts 114)))))

(defn perfect-square? [n]
  (zero? (second (exact-integer-sqrt n))))

(def irrational-squares
     (filter (fn [n] (not (rational? (sqrt n)))) (iterate inc 1)))

(def not-perfect-squares 
     (filter #(not (perfect-square? %)) (iterate inc 1)))

(defn count-odd-periods [N]
  (count (filter odd-period? 
		 (map #(contiuned-fraction-for-sqrts %) 
		      (take-while #(<= % N) irrational-squares)))))

(defn count-odd-periods-b [N]
  (count (filter odd-period? 
		 (map #(contiuned-fraction-for-sqrts %) 
		      (take-while #(<= % N) not-perfect-squares)))))

(deftest test-count-odd-periods
  (is (= 4 (count-odd-periods 13)))
  (is (= 4 (count-odd-periods-b 13)))
)

;; user> (time (count-odd-periods 10000))
;; "Elapsed time: 8357.71347 msecs"
;; 1322
;; 
;; user> (time (count-odd-periods-b 10000))
;; "Elapsed time: 7726.570798 msecs"
;; 1322


