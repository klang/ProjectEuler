(ns problem070
  (:use 
   [clojure.test :only (deftest is)]
   [tools.primes :only (totient)]
   [tools.numbers :only (digits)]))

(defn totient-permutation? [number]
  (= (sort (digits (totient number))) 
     (sort (digits number))))

(deftest test-problem-070
  (is (= (set (digits (totient 87109))) (set (digits 79180))))
  (is (= (set (digits (totient 87109))) (set (digits 79180))))
  (is (totient-permutation? 87109)))

(def totient-permutations 
     (filter #(totient-permutation? %) (iterate inc 2)))

; brute force .. 
(defn n-for-min-totient-permutation-ratios [limit]
  (loop [tp totient-permutations
	 currentN (first tp) 
	 currentMin (/ (first tp) (totient (first tp)))]
    (if (or (empty? tp) ( < limit (first tp)))
      (list currentN currentMin)
      (let [thisRatio (/ (first tp) (totient (first tp)))]
	(if (<= thisRatio currentMin)
	  (do (println (list (first tp) thisRatio)) (recur (rest tp) (first tp) thisRatio ))
	  (do (println (list (first (rest tp)))) (recur (rest tp) currentN currentMin)))))))

;; user> (time (n-for-min-totient-permutation-ratios 10000000) )
;; (5380657 5380657/5375860)
;; (6018163 6018163/6013168)
;; (6636841 6636841/6631684)
;; (7026037 7026037/7020736)
;; (7357291 7357291/7351792)
;; (7507321 7507321/7501732)
;; (8316907)
;; (8316907 8316907/8310976)
;; (8319823 8319823/8313928)
;; (8326170)
;; ---
;; (9970632)
;; (9983167)
;; (10101409)
;; "Elapsed time: 5498767.682737 msecs"
;; (8319823 8319823/8313928)
;; 
(defn problem070 [] 0)
