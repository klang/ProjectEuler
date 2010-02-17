;; http://en.wikipedia.org/wiki/Partition_(number_theory)

(use 'clojure.contrib.math)
;; floor

;; intermediate function
(defn p [k n]
  "direct implementation of term (54) on http://mathworld.wolfram.com/PartitionFunctionP.html"
  (cond 
    (> k n) 0
    (= k n) 1
    :else (+ (p (+ k 1) n) (p k (- n k)))))

(def p (memoize p))

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
(defn pentagonal        [n] (quot (- (* 3 n n) n) 2))
(defn second-pentagonal [n] (quot (+ (* 3 n n) n) 2))

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

(def pn (memoize pn))

(defn partitions [n]
  "returns the partitions of n as a list")

(def p10 
     "lazy sequence returning partitions of p(10)"
     (partitions 10))

(defn partitions# [n]
  "returns the number of partitions of n"
  (pn n))



(defn partitions [n]
  "returns the partitions of n as a list")
;; http://www.site.uottawa.ca/~ivan/F49-int-part.pdf
;; ZS1
(defn int_part [n]
  (let [x (assoc (vec (take n (repeat 1))) 0 n ) ; [n 1 1 1 .. 1]
	m 1 h 0                                  ; vectors index at 0
	output (conj '() (x 0))
	]
    (do (println output))
    (while (not (= 1 (x 0)))
      (if (= (x h) 2)
	(do (assoc x h 1) (inc m) (dec h))
	(let [r (- (x h) 1)
	      t (- m (+ h 1))
	      (assoc x h r)]
	  (while (>= t r)
	    (do (inc h) (assoc x h r) ())))
	
	)
      )
    x
    )
  )
;; def int_part(n):
;;     x=[1 for i in range(n+1)]
;;     x[1]=n; m=1; h=1
;;     yield [x[1]]
;;     while x[1]!=1:
;;         if x[h]==2:
;;             m+=1; x[h]=1; h-=1
;;         else:
;;             r=x[h]-1; t=m-h+1; x[h]=r
;;             while t>=r:
;;                 h+=1; x[h]=r; t-=r
;;                 if t==0: m=h
;;                 else: m=h+1
;;                 if t>1:
;;                     h+=1; x[h]=t
;;         yield x[1:m+1]
;; 


(defn my-combinations
  "If m=1, generate a nested list of numbers [0,n)
   If m>1, for each x in [0,n), and for each list in the recursion on [x+1,n), cons the two"
  [m n]
  (letfn [(comb-aux
	   [m start]
	   (if (= 1 m)
	     (for [x (range start n)]
	       (list x))
	     (for [x (range start n)
		   xs (comb-aux (dec m) (inc x))]
	       (cons x xs))))]
    (comb-aux m 0)))
 
(defn print-combinations
  [m n]
  (doseq [line (my-combinations m n)]
    (doseq [n line]
      (printf "%s " n))
    (printf "%n")))