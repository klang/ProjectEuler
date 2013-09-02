(ns problem078
  (:use [clojure.math.numeric-tower :only (expt)]))

;; http://www.research.att.com/~njas/sequences/A000041
;; F. Ruskey, The first 284547 partition numbers (52MB compressed file
;; http://www.theory.cs.uvic.ca/tables/partitions.txt.gz
;;
;; grep -n '000000$' partitions.txt 
;; 55374:36325300925435785930832331577396761646715836173633893227071086460709268608053489541731404543537668438991170680745272159154493740615385823202158167635276250554555342115855424598920159035413044811245082197335097953570911884252410730174907784762924663654000000

;; algorithm from problem 76 can be modified to calculate the result .. 

;; http://www.ces.clemson.edu/~kevja/REU/2002/JDavisAndEPerez.pdf
(defn pentagonal [n] 
  (quot (- (* 3 n n) n) 2))
(defn second-pentagonal [n] 
  (quot (+ (* 3 n n) n) 2))

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

(defn problem078 [] 0)
