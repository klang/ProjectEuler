(ns tools.farey
  (meta {:description "A surprisingly simple algorithm exists to generate the terms in either traditional order (ascending) or non-traditional order (descending). 
The algorithm computes each successive entry in terms of the previous two entries using the mediant property given above. If a/b and c/d are the two given entries, and p/q is the unknown next entry, then c/d = (a+p)/(b+q). c/d is in lowest terms so there is an integer k so that kc=a+p and kd = b+q, giving p = kc-a and q = kd-b. The value of k must give a value of p/q which is as close as possible to c/d, which implies that k must be as large as possible subject to kd-b ≤ n, so k is the greatest integer ≤ (n+b)/d." 
	 :link "http://en.wikipedia.org/wiki/Farey_sequence"}))

(defn farey [n]
  (map #(/ (first %) (second %)) 
       (iterate 
	(fn [[a b c d]] 
	  (let [k (int (/ (+ n b) d))] 
	    [c d (- (* k c) a) (- (* k d) b)])) [0 1 1 n])))

(defn farey-seq [n]
  "returns a farey sequence except the "
  (map #(list (first %) (second %)) 
       (lazy-cat
	(take-while (fn [[a b c d]] (<= c n))
		    (iterate 
		     (fn [[a b c d]] 
		       (let [k (int (/ (+ n b) d))] 
			 [c d (- (* k c) a) (- (* k d) b)])) [0 1 1 n]))
	(list (list n n)))))

(defn fraction-compare [[n d] [cn cd]]
  "a cheap way to compare fractions returned by farey-seq"
  (not (and (= n cn) (= d cd))))

(comment
(drop-while #(fraction-compare % [1 3])
	    (take-while #(fraction-compare % [1 2])
			(farey-seq 8)))
)

(comment
  (defn farey-asc [n]
    (loop [a 0, b 1, c 1, d n]
      (if (< c n)
	(recur  ))))

  (defn farey-dec [n]
    (let [a 1, b 1, c (- n 1), d n])))