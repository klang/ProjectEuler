(ns problem049
  (meta {:description "
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence."})
  (:use 
   [tools.primes :only (primes)]
   [tools.numbers :only (digit-set)])
  (:require
   [clojure.math.combinatorics :only (combinations) :as comb]))


;; What 12-digit number do you form by concatenating the three terms in this sequence?

(def four-digit-primes
     (filter #(< 1000 %) (take-while #(< % 10000) primes)))
;; user> (count four-digit-primes)
;; 1061

(defn spacing [[ d1 d2 d3]]
  ;; if d1 < d2 < d3
  (= (- d2 d1) (- d3 d2))
  ;; will show that the they are equally spaced apart
  )

;; sort by digits
;; 
;;  key (set)      value (vector)
;; {#{1 2 3}       [123 321 213 312]
;;  #{1 2}         [12 21 11 22 1 2]
;;  ...}
;; 
(def a {#{1 2 3}       [123 321 213 312]
	#{1 2}         [12 21 11 22 1 2]})
(def a (assoc a #{3 1} (vec (conj (a #{1 3}) 31))))
(def a (assoc a #{4 1} (vec (conj (a #{1 4}) 41))))
(def a (assoc a #{4 2} (vec (conj (a #{2 4}) 42))))

(def b {})
(def b (assoc b #{3 1} (vec (conj (b #{1 3}) 31))))

;; --- figuring out how to use refs
(def counter (ref 0))
(defn next-counter [] (dosync (alter counter inc)))
(defn next-counter-with [n] (dosync (alter counter #(+ % n))))
;; @counter becomes the first argument for the update function, i.e. %
;;----
(def sorted-primes (ref {}))
(defn add-prime [number]
  (dosync (alter sorted-primes 
		 #(assoc % (digit-set number) 
			 (vec (conj (% (digit-set number)) number))))))
;;----

(defn prime-sorter []
  (let [sorted-primes (ref {})]
    (loop [ps (filter #(< 1000 %) (take-while #(< % 10000) primes))]
      (if (= '() ps)
	sorted-primes
	(let [number (first ps)]
	  (dosync (alter sorted-primes 
		 #(assoc % (digit-set number) 
			 (vec (conj (% (digit-set number)) number)))))
	  (recur (rest ps)))))))


(def sorted-primes (prime-sorter))

;; @sorted-primes
;; <<snipped  #{4 7 8 9} [4789 4987 7489], #{5 6 8 9} [5689 5869],  snipped>>
;; looking good..

;;user> (count @sorted-primes)
;;236

;; there are 165 potential terms with more than 3 4-digit primes
;; user> (count (filter #(< 2 (count (second %))) @sorted-primes))
;; 165

;; finding the example given:
;; user> (@sorted-primes #{1 4 7 8})
;; [1487 1847 4817 4871 7481 7841 8147 8741]
;; (filter #(spacing %) (comb/combinations (@sorted-primes #{1 4 7 8}) 3))
;; ((1487 4817 8147))
(defn list-spacing [[ d1 d2 d3]]
  (list (- d2 d1) (- d3 d2)))

(def foo (map #(comb/combinations (second %) 3) (filter #(< 2 (count (second %))) @sorted-primes)))

(defn find-sequence [sorted-primes]
  (loop [sp @sorted-primes
	 found []]
    (if (= '() sp) 
      found
      (let [item (filter #(spacing %) (comb/combinations (second (first sp)) 3))]
	(recur (rest sp) (if (= '() item) found (conj found item))))
      )
    ))
;; user> (find-sequence sorted-primes)
;;[((2969 6299 9629)) ((1487 4817 8147))]
;;user> (list-spacing '(2969 6299 9629))
;;(3330 3330)

;; -- ok, I search for any combination with any kind of equal spacing..

(defn problem049 []
  (. Long parseLong (apply str (first (first (find-sequence sorted-primes))))))
