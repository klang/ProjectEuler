(ns problem044
  (meta {:description "Pentagonal numbers are generated by the formula, P(n)=n*(3n-1)/2. The first ten pentagonal numbers are:

1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...

It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference, 70 - 22 = 48, is not pentagonal.

Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference is pentagonal and D = |Pk - Pj| is minimised; what is the value of D?"})
  (:use clojure.contrib.math)
  (:use clojure.test)
  )

(def pentagonals ; (quot (* n (- (* 3 n) 1)) 2)
  (map (fn [n] (quot (- (* 3 n n) n) 2)) (iterate inc 1)))
(defn pentagonal [n] (quot (- (* 3 n n) n) 2))

; P(n)=n*(3n-1)/2
;; j*(3j-1)/2 - k*(3k-1)/2 = n*(3n-1)/2
;; and
;; j*(3j-1)/2 + k*(3k-1)/2 = m*(3m-1)/2

(defn p [n] (- (* 3 n n) n))
(def p (memoize p))
(defn pdif [j k] (- (p k) (p j)))
(defn psum [j k] (+ (p j) (p k)))


(defn parabola [a b c]
  (let [D (- (* b b) (* 4 a c))
	sqrtD (exact-integer-sqrt D)
	minus-b (* -1 b)
	two-a (* 2 a)
	] {:D D
	   :sqrtD sqrtD
	   :r1 (/  (+ minus-b (first sqrtD)) two-a)
	   :r2 (/  (- minus-b (first sqrtD)) two-a)}))
    ;(zero? (exact-integer-sqrt (- 1 (* 12 c))))

; (parabola 3 -1 (* -2 70))
; (parabola 3 -1 (* -2 7))

(defn pentagonal? [c]
  (let [D (- 1 (* -24 c))
	sqrtD (exact-integer-sqrt D)
	] {:D D 
	   :sqrtD sqrtD 
	   :r1 (/  (+ 1 (first sqrtD)) 6) 
	   :r2 (/  (- 1 (first sqrtD)) 6)
	   :pentagonal? (and (zero? (second sqrtD))
			     (integer? (/  (+ 1 (first sqrtD)) 6)))}))

(defn pentagonal? [c]
  (let [sqrtD (exact-integer-sqrt (- 1 (* -24 c)))]
      (and (zero? (second sqrtD))
	   (integer? (/  (+ 1 (first sqrtD)) 6)))))

(deftest test-pentagonal
  (is (pentagonal? 1))
  (is (pentagonal? 5))
  (is (not (pentagonal? 7)))
  (is (pentagonal? 12))
  (is (pentagonal? 22))
  (is (not (pentagonal? 57)))
  (is (pentagonal? 70))
  (is (pentagonal? 92)))

;; the pentagonal number, if pentagonal? returns true
(defn pentagonal-n [c]
  (let [sqrtD (exact-integer-sqrt (- 1 (* -24 c)))]
    (if (and (zero? (second sqrtD))
	   (integer? (/  (+ 1 (first sqrtD)) 6)))
      (/  (+ 1 (first sqrtD)) 6)
      false)))

(defn pdiff [n] (take-while #(not (zero? %)) (map #(- n %) pentagonals)))
(defn psum [n] (take-while #(not (zero? %)) (map #(+ n %) pentagonals)))
;;(take 10 (map #(pdiff %) pentagonals))
;;(map println (take 10 (map #(pdiff %) pentagonals)))

(comment (apply str (map println (take 10 (map #(pdiff %) pentagonals)))))
;; (1 5 12 22 35 51 70 92 117 145)
;; ()
;; (4)
;; (11 7)
;; (21 17 10)
;; (34 30 23 13)
;; (50 46 39 29 16)
;; (69 65 58 48 35 19)
;; (91 87 80 70 57 41 22)
;; (116 112 105 95 82 66 47 25)
;; (144 140 133 123 110 94 75 53 28)


;;(take 10 (map #(filter pentagonal? %) (map #(pdiff %) pentagonals)))

(defn pdiff [n] (take-while #(not (zero? %)) (map #(- n %) pentagonals)))
(defn pdiff [n] (take-while #(< 0 %) (map #(- n %) pentagonals)))
;(defn pdiff [j] (take (- j 1) (map #(- (pentagonal j) %) pentagonals)))
(comment (apply str (map println (take 10 (map #(pdiff %) pentagonals)))))

(def indexed-pentagonals 
     (map (fn [n] [n (quot (- (* 3 n n) n) 2)]) (iterate inc 1)))
(def mapped-pentagonals 
     (map (fn [n] [(quot (- (* 3 n n) n) 2) n]) (iterate inc 1)))

(take 100 (map #(filter pentagonal? %) (map #(pdiff %) pentagonals)))
(take 100 (map #(if (pentagonal? %) % 0) (map #(pdiff %) pentagonals)))

(def mpent (into {} (take 100 mapped-pentagonals)))

;; (defn pdiff [n] (take-while #(< 0 %) (map #(- n %) pentagonals)))
;; (take 100 (map #(filter pentagonal? %) (map #(pdiff %) pentagonals)))
;; (take 10 (filter not-empty (map #(filter pentagonal? %) (map #(pdiff %) pentagonals))))

(defn pentagonal-diff [n] (take-while #(< 0 (:diff %)) (map #(hash-map :pentagonal n :pj n :pk % :diff (- n %)) pentagonals)))

;(map (fn [s] (if (pentagonal? (:diff s)) s) ) (pentagonal-diff 70))
;(filter #(not (nil? %))  (map (fn [s] (if (pentagonal? (:diff s)) s) ) (pentagonal-diff 70)))

(defn pentagonal-diff-and-sum [n]
  (map #(hash-map :pentagonal (:pentagonal %) :diff (:diff %) :sum (+ (:pentagonal %) n)) (pentagonal-diff n)))

(defn pentagonal-diff [n] (take-while #(< 0 (:diff %)) (map #(hash-map :pentagonal n :pj n :pk % :diff (- n %)) pentagonals)))
(defn pentagonal-diff-and-sum [n]
  (map #(conj % {:sum (+ (:pentagonal %) n)}) (pentagonal-diff n)))


(def pdas (filter not-empty (map #(pentagonal-diff-and-sum %) pentagonals)))
; (take 10 (map #(pentagonal-diff-and-sum %) pentagonals))

;(filter #(not (nil? %))  (map (fn [s] (if (pentagonal? (:diff s)) s) ) (pentagonal-diff 70)))
;(filter #(not (nil? %))  (map (fn [s] (if (pentagonal? (:diff s)) s) ) (pentagonal-diff n)))
(defn pentagonal-diff-and-sum-reduced [n]
  (map #(hash-map :pentagonal (:pentagonal %) :diff (:diff %) :sum (+ (:pentagonal %) n)) 
       (filter #(not (nil? %))  
	       (map (fn [s] (if (pentagonal? (:diff s)) s) ) (pentagonal-diff n)))))



(defn pentagonal-diff [n] (take-while #(< 0 (:diff %)) (map #(hash-map :pj n :pk % :diff (- n %)) pentagonals)))
(defn pentagonal-diff-and-sum [n]
  (map #(conj % {:sum (+ (:pj %) (:pk %))}) (pentagonal-diff n)))

;; http://gist.github.com/391238
(defn flatten [s] (remove seq? (tree-seq seq? seq s)))

;(defn pentagonal-diff-and-sum-reduced [n]
;  (map #(conj % {:sum pj} )) 
;       (filter #(not (nil? %))  
;	       (map (fn [s] (if (pentagonal? (:diff s)) s) ) (pentagonal-diff n)))))

(defn pentagonal-data [n] (take-while #(< 0 (:diff %)) (map #(hash-map :pj n :pk % :diff (- n %) :sum (+ n %)) pentagonals)))
(defn pentagonal-diff [n] (filter #(pentagonal? (:diff %)) (pentagonal-data n)))
(defn pentagonal-sum [n] (filter #(pentagonal? (:sum %)) (pentagonal-data n)))

(defn pentagonal-diff-and-sum [n] (filter #(and (pentagonal? (:diff %)) (pentagonal? (:sum %))) (pentagonal-data n)))

(comment
  (take 1 (flatten (filter not-empty (map #(pentagonal-diff %) pentagonals))))
  (take 1 (flatten (filter not-empty (map #(pentagonal-sum %) pentagonals))))
  (take 1 (flatten (filter not-empty (map #(pentagonal-diff-and-sum %) pentagonals))))
 )
(defn foo []
  (letfn [(pentagonal-data [n] (take-while #(< 0 (:diff %)) 
					   (map #(hash-map :pj n :pk % :diff (- n %) :sum (+ n %))
						pentagonals)))
	  (pentagonal-sum [n] (filter #(pentagonal? (:sum %)) 
				      (pentagonal-data n)))	  
	  ]
    (loop [candidates (flatten (filter not-empty (map #(pentagonal-sum %) pentagonals)))
	   i 0]
      (do  #_(println i (first candidates))
	   (if (pentagonal? (:diff (first candidates)))
	     (first candidates)
	     (recur (rest candidates) (inc i))))
      )))
;; not the minimum .. simply the first
;; {:pk 1560090, :pj 7042750, :diff 5482660, :sum 8602840}
(defn problem044 []
  (let [bar (foo)]
    (bar :diff)))
