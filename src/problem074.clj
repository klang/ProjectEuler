(ns problem074
  (meta {:description "The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:

1! + 4! + 5! = 1 + 24 + 120 = 145

Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169; it turns out that there are only three such loops that exist:

169 -> 363601 -> 1454 -> 169
871 -> 45361 -> 871
872 -> 45362 -> 872

It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,

69 -> 363600 -> 1454 -> 169 -> 363601 (-> 1454)
78 -> 45360 -> 871 -> 45361 (-> 871)
540 -> 145 (-> 145)

Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain with a starting number below one million is sixty terms.

How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?"})
  (:use tools.numbers))

;(defn factorial [n]   (reduce * (range n 0 -1)))
(def factorial-mem (memoize factorial))

(defn step [n]
  (reduce + (map #(factorial-mem %) (digit-list n))))

(defn chain [n]
  "naïve calculation of the factorial chain (does a lot of recalculation)"
  (loop [item (step n) seen (sorted-set n)]
    (if (seen item) 
      (count seen)
      (recur (step item) (conj seen item)))))

(defn chains [n limit]
  (count (filter #(= n (chain %)) (range 1 limit))))

(defn chains [n start limit]
  (count (filter #(= n (chain %)) (range start limit))))

(defn chain-list [n]
  "naïve calculation of the factorial chain (does a lot of recalculation)"
  (loop [item (step n) seen (sorted-set n) sequence [n]]
    (if (seen item)
      sequence
      (recur (step item) (conj seen item) (conj sequence item)))))

(defn chains-list [n limit]
  (filter #(= n (count (chain %))) (range 1 limit)))

;; problem074> (time (chains 60 100000))
;; "Elapsed time: 48190.659005 msecs"
;; 42
;; problem074> (time (chains 60 100000 200000))
;; "Elapsed time: 48172.513403 msecs"
;; 0
;; problem074> (time (chains 60 200000 500000))
;; "Elapsed time: 147024.989296 msecs"
;; problem074> (time (chains 60 500000 1000000))
;; "Elapsed time: 252445.451798 msecs"
;; 120
;; (+ 42 0 240 120) : 402

