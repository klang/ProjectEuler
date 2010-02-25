(load "tools")

(defn n-for-max-totient-ratios [limit]
  (loop [n 1 m 1
	 tots (map (fn [n] [n (/ n (totient n))]) 
		   (take-while #(<= % limit) 
			       (iterate inc 2)))]
    (if (empty? tots)
      (list n m)
      (if (< m (second (first tots)))
	(recur (first (first tots)) 
	       (second (first tots)) 
	       (rest tots))
	(recur n m (rest tots))))))

;; user> (time (n-for-max-totient-ratios 1000000))
;; "Elapsed time: 216187.453262 msecs"
;; (510510 17017/3072)

;; n/phi(n) = 2n/phi(2n)
;; if n > limit/2, n is the answer

;; optimating by simply finding the number with most unique prime factors

(defn n-for-max-totient-ratios [n]
  (loop [prod 1 p primes]
    (if (> (* prod (first p)) n)
      (list prod (/ prod (totient prod)))
      (recur (* prod (first p)) (rest p)))))

;; user> (time (n-for-max-totient-ratios 1000000))
;; "Elapsed time: 10.455521 msecs"
;; (510510 17017/3072)
