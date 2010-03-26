;; http://en.wikipedia.org/wiki/Partition_(number_theory)
(ns tools.partitions 
  (:use clojure.contrib.math))

;; intermediate function
(defn p [k n]
  "direct implementation of term (54) on http://mathworld.wolfram.com/PartitionFunctionP.html"
  (cond 
    (> k n) 0
    (= k n) 1
    :else (+ (p (+ k 1) n) (p k (- n k)))))

;(def p (memoize p))

;   n    0  1  2  3  4  5   6   7   8   9  10  11  12   13   14   15   16   17   18   19
; p(n)   1  1  2  3  5  7  11  15  22  30  42  56  77  101  135  176  231  297  385  490

(defn upper-bound [k]
  "Ramanujan's upper bound for number of partitions of k"
  (int (/ (Math/exp (* Math/PI (sqrt (/ (* 2 k) 3)))) (* 4 k (sqrt 3)))))

;; http://www.cs.sunysb.edu/~algorith/files/generating-partitions.shtml
(def pentagonals ; (quot (* n (- (* 3 n) 1)) 2)
  (map (fn [n] (quot (- (* 3 n n) n) 2)) (iterate inc 1)))
(def second-pentagonals ; (quot (* n (+ (* 3 n) 1)) 2)
  (map (fn [n] (quot (+ (* 3 n n) n) 2)) (iterate inc 1)))

;; http://www.ces.clemson.edu/~kevja/REU/2002/JDavisAndEPerez.pdf
(defn pentagonal [n] 
  (quot (- (* 3 n n) n) 2))
(defn second-pentagonal [n] 
  (quot (+ (* 3 n n) n) 2))

;http://home.att.net/~numericana/answer/numbers.htm#partitions
;; input m
;; dim p(m)
;; p(0) = 1
;; 
;; for i = 1 to m
;;   j=1 : k=1 : s=0
;;   while j>0
;;     j = i-(3*k*k+k)\2                # second-pentagonal
;;     if j>=0 then s = s - (-1)^k*P(j) 
;;     j = i-(3*k*k-k)\2                # pentagonal
;;     if j>=0 then s = s - (-1)^k*P(j) 
;;     k = k+1
;;   wend
;;   p(i) = s
;; next i

(defn pn [n]
  "direct implementation of term (11) on http://mathworld.wolfram.com/PartitionFunctionP.html"
  (cond 
    (< n 0) 0
    (= n 0) 1
    :else 
    (reduce + (map 
	       (fn [k] (* (expt -1 (+ k 1)) 
			  (+ (pn (- n (pentagonal k)))
			     (pn (- n (second-pentagonal k))))))
	       (range 1 (+ n 1))))))

;(def pn (memoize pn))

(defn partitions [n]
  "returns the partitions of n as a list")

;; "lazy sequence returning partitions of p(10)"
(def p10 
     (partitions 10))

(defn partitions# [n]
  "returns the number of partitions of n"
  (pn n))


(defn partitions [n pool]
  "pool of numbers to take from when partitioning n. pool could be primes"
  nil)

(defn partitions [n]
  "returns the partitions of n as a list")
;; http://www.site.uottawa.ca/~ivan/F49-int-part.pdf
;; Fast Algorithms for generating integer partitions
;; Antoine Zoghbi and Ivan Stojmenović
;; ZS1 - anti-lexicographic order 
;; ([5], [4 1], [3 2], [3 1 1], [2 2 1], [2 1 1 1], [1 1 1 1 1])
;; ZS1 - anti-lexicographic order
;; (defn x [n] (assoc (vec (take n (repeat 1))) 0 n)) 	;; x  [n 1 1 1 .. 1]
;; 
(comment
  (let [n 5
	x (assoc (vec (take n (repeat 1))) 0 n )
	m 0 
	h 0]
    (loop [r 2 t 3] 
      (when (> t r) 
	(do (inc h) 
	    (assoc x h r) 
	    (dec t) 
	    (do (println (list r t x)))
	    )))))
(comment
  (defn zs1 [n]
    (loop [x (assoc (vec (take n (repeat 1))) 0 n ), m 0, h 0, output (conj [] (x 0))]
      (do (println (x 0)))
      (if (= (x 0) 1) 
	output
	(if (= (x h) 2)
	  (list (assoc x h 1) (inc m) (dec h) (conj output (subvec x 0 (+ m 1))))
	  (list (assoc x h 1) (inc m) (dec h) (conj output (subvec x 0 (+ m 1))))))
      )
    ))
(comment 
  (defn ZS1 [n]
    (loop [x (assoc (vec (take n (repeat 1))) 0 n ), m 0, h 0, output (conj [] (x 0)) ]
					;    (do (println (x 0)))
      (if (= (x 0) 1) ;; the last iteration will be [1 1 1 1 .. 1] 
	output
	(if (= (x h) 2)
					;             x           m       h        output            
	  (recur (assoc x h 1) (inc m) (dec h) (conj output (subvec x 0 (+ m 1))))
	  (let [r (- (x h) 1) ;; r = x[h]-1
		t (- m (- h 1))	;; t = m-h+1 
		x (assoc x h r)	;; x[h]=r         x should be transient for this to make sense
		]

	    (while ())
	    (if (zero? t))
					;             x           m       h        output            
	    (recur     (if (zero? t) h (inc h)) (if (> t 1)) (conj output (subvec x 0 m)) )
	  
	    )
	  )
	)

      (do (println (subvec x 0 m)))
      )))

(defn init-vector [n] (assoc (vec (take n (repeat 1))) 0 n ))
(defn init-vars [n] (list :x1 ((init-vector n) 0) :m 1 :h 1))
(defn change-item-i [x i v] (assoc x i v))
(def x (init-vector 5))
(def m 1)
(def h 1)
(comment
  (defn zs1 [n]
    (loop [x (transient (assoc (vec (take n (repeat 1))) 0 n)) ; [n 1 1 1 ... 1]
	   output (transient [[(x 0)]])			       ; [[n]]
	   m 0 h 0]
      (if (= (x 0) 1) ;; the last iteration will be [1 1 1 1 ... 1]
	(persistent! output)
	(if (= (x h) 2) 
	  ;;     x[h] = 1      output: x[1]..x[m]           m=m+1   h=h-1
	  (recur (assoc x h 1) (conj output (subvec x 0 m)) (+ m 1) (- h 1))
	  (let [r (- (x h) 1)		; r=x[h]-1
		]
	    (loop [h h
		   t (+ (- h m) 1)	; t=m-h+1
		   x (assoc x h r) ; x[h]=r      .. make the template for the next run
		   ]
	      (do (println (list h t x)))
	      (if (and (>= t r) (<= h 5)) ; (<= h 5) guard to control the recursion
		(recur (+ h 1) (- t r) (assoc x (+ h 1) r))
		)))
	  )))))









