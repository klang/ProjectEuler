(load "tools")

;; we are going to calculate a lot of values of this type
(defn one-minus-one-over-n [n] (- 1 (/ 1 n)))
;; which is why we will memoize them
(def one-minus-one-over-n (memoize one-minus-one-over-n))

(defn totient [n]
  "totient(n) = n * (1 - 1/p1)(1 - 1/p2)(1 - 1/p3)...(1 - 1/pm) 
where p1...pm are the unique prime factors of n."
  (* n (reduce * (map #(one-minus-one-over-n %) (set (prime-factors n))))))

(def totient (memoize totient))

(defn totient-ratio [n] (/ n (totient n)))
(def totient-ratio (memoize totient-ratio))

(def totient-ratios
     (map #(totient-ratio %) (iterate inc 2)))

(defn max-totient-ratio [limit]
  (reduce max (map #(totient-ratio %) 
		   (take-while #(<= % limit) (iterate inc 2)))))

(defn n-for-max-totient-ratios [limit]
  (loop [n 1 m 1
	 tots (map (fn [n] [n (totient-ratio n)]) 
		   (take-while #(<= % limit) 
			       (iterate inc 2)))]
    (if (empty? tots)
      (list n m)
      (if (< m (second (first tots)))
	(recur (first (first tots)) 
	       (second (first tots)) 
	       (rest tots))
	(recur n m (rest tots))))))

;; out of memory at (n-for-max-totient-ratios 300000)

;; user> (count (take-while #(< % 1000000) primes))
;; 78498
