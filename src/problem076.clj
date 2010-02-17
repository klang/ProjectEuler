;; It is possible to write five as a sum in exactly six different ways:
;; 
;; 4 + 1
;; 3 + 2
;; 3 + 1 + 1
;; 2 + 2 + 1
;; 2 + 1 + 1 + 1
;; 1 + 1 + 1 + 1 + 1
;; 
;; How many different ways can one hundred be written as a sum of at least two positive integers?
;(load "partitions")

;; http://en.wikipedia.org/wiki/Integer_partition
;; 190569292 - 1
;; simple lookup, took less than a minute :-)

(defn p [k n] 
  (cond 
    (> k n) 0
    (= k n) 1
    :else
    (+ (p (+ k 1) n) (p k (- n k))))
  )
;; user> (time (p 1 100))
;; "Elapsed time: 1209134.242555 msecs"
;; 190569292
;; 
;; obviously not the best way to do this

(def p (memoize p))

;; user> (time (p 1 100))
;; "Elapsed time: 82.583742 msecs"
;; 190569292

;; .. ok, that was quick
 
;; Term number (11) here http://mathworld.wolfram.com/PartitionFunctionP.html

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

;; http://home.att.net/~numericana/answer/numbers.htm#partitions
;; gives this very fast algorithm ..
;; (basically, it remebers the previous terms, which is what we do with memoize above)

;; input m
;; dim p(m)
;; p(0) = 1
;; 
;; for i = 1 to m
;;   j=1 : k=1 : s=0
;;   while j>0
;;     j = i-(3*k*k+k)\2
;;     if j>=0 then s = s - (-1)^k*P(j)
;;     j = i-(3*k*k-k)\2
;;     if j>=0 then s = s - (-1)^k*P(j)
;;     k = k+1
;;   wend
;;   p(i) = s
;; next i


