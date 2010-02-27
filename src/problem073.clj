;; Consider the fraction, n/d, where n and d are positive integers. If nd and HCF(n,d)=1, it is called a reduced proper fraction.
;; 
;; If we list the set of reduced proper fractions for d <  8 in ascending order of size, we get:
;; 
;; 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
;; 
;; It can be seen that there are 3 fractions between 1/3 and 1/2.
;; 
;; How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d <= 12,000?
;; 
;; Note: The upper limit has been changed recently.
;; 
(load "tools")
(use 'clojure.contrib.math)

(defn fractions [min max limit] 
  (sort (for [d (range 1 (+ limit 1)) 
	      n (range 1 d) 
	      :when (and (= 1 (gcd n d)) 
			 (< min (/ n d) max))] 
	  (/ n d))))

(deftest test-fractions
   (is (= 3) (count (fractions 1/3 1/2 8))))

;; user> (time (count (fractions 1/3 1/2 1000)))
;; "Elapsed time: 5334.214105 msecs"
;; 50695
;; .. method not efficient for target limit (takes way more than a minute)

; (+ 1/3 1/12000)

(defn find-min [a b N]
  "Implements the algorithm presented in 071_overview.pdf" 
  (loop [bestNum 0
	 bestDenom 1
	 Ns (range 2 N)
	 n 0]
    (if (empty? Ns)
      (list (/ bestNum bestDenom) n)
      (let [currDenom (first Ns)
	    currNum (quot (- (* a currDenom) 1) b)]
	(if (< (* bestNum currDenom) (* currNum bestDenom))
	  (recur currNum currDenom (rest Ns) (if (< 1/2 currNum) (inc n)))
	  (recur bestNum bestDenom (rest Ns) (inc n))
	  )))))

;; (sort (for [d (range 1 8) n (range 1 d) :when (= 1 (gcd n d))] (/ n d)))
;; (find-max 3 7 8)

;; user> (time (find-max 3 7 1000000))
;; "Elapsed time: 1867.093735 msecs"
;; 428570/999997
