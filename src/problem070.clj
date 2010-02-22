(load "tools")
(use 'clojure.contrib.math 'clojure.set)

(defn totient-permutation? [number]
  (= (set (digits (totient number))) 
     (set (digits number))))

(defn totient-permutation? [number]
  (= (sort (digits (totient number))) 
     (sort (digits number))))

(deftest test-problem-070
  (is (= (set (digits (totient 87109))) (set (digits 79180))))
  (is (= (set (digits (totient 87109))) (set (digits 79180))))
  (is (totient-permutation? 87109)))

;; user> (time (count (range 1 (expt 10 7))))
;; "Elapsed time: 7230.823581 msecs"
;; 9999999

;(reduce min (map #(/ (- % 1) %) (take 100 primes)))
; (totient p) = (- p 1) for p in primes

;(every? #(and true %) (map #(totient-permutation?%) (take 100 primes)))
;false

(def totient-permutations 
     (filter #(totient-permutation? %) (iterate inc 2)))

(def totient-ratios
     (map #(/ % (totient %)) totient-permutations))

(defn min-totient-ratios [limit]
  (reduce min (map #(/ % (totient %)) 
		   (take-while #(< % limit) totient-permutations))))

(def totient (memoize totient))

(def tp (take-while #(< % (expt 10 7)) totient-permutations))
;; user> (time (count tp))
;; "Elapsed time: 0.372115 msecs"
;; 200

;; now we are cooking!

(def tpr (map #(/ % (totient %)) tp))

(defn n-for-min-totient-permutation-ratios []
  (loop [n 10 m 10
	 tots (map (fn [n] [n (/ n (totient n))]) tp)]
    (if (empty? tots)
      (list n m)
      (if (> m (second (first tots)))
	(recur (first (first tots)) 
	       (second (first tots)) 
	       (rest tots))
	(recur n m (rest tots))))))

;; user> (n-for-min-totient-permutation-ratios)
;; (400399 400399/399040)
;; wrong (somehow tp was not calculated again after changing limit)
