;; It is possible to show that the square root of two can be expressed as an infinite continued fraction.
;; 
;; sqrt 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
;; 
;; By expanding this for the first four iterations, we get:
;; 
;; 1 + 1/2 = 3/2 = 1.5
;; 1 + 1/(2 + 1/2) = 7/5 = 1.4
;; 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
;; 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
;; 
;; The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.
;; 
;; In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?

(load "problem065")

;; (estimate-continued-fraction [1 2])
;; (estimate-continued-fraction [1 2 2])
;; (estimate-continued-fraction [1 2 2 2])
;; (estimate-continued-fraction [1 2 2 2 2])
;; (estimate-continued-fraction [1 2 2 2 2 2])
;; (estimate-continued-fraction [1 2 2 2 2 2 2])

(defn p57 [] 
  (loop [sq2c [1 2] i 1 c 0]
    (if (< 1000 i)
      c
      (let [est (estimate-continued-fraction sq2c)
	    num (count (digits (.numerator est)))
	    den (count (digits (.denominator est))) 
	    ]
	;(do (println (list num den i)))
	(recur (conj sq2c 2) (inc i) (if (< den num) (inc c) c))))))

;; user> (time (p57))
;; "Elapsed time: 170892.443036 msecs"
;; 153
