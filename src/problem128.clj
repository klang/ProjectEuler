(ns problem128
  (:use [tools.primes :only (prime?)]
        [clojure.math.numeric-tower :only (abs)]))

(defn diff-prime? [a b] (prime? (abs (- a b))))

(def ring 
  [[1]
   [2 3 4 5 6 7]
   [8 9 10 11 12 13 14 15 16 17 18 19]
   [20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37]
   [38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61]])


(defn tile [n]
  )

{(first (ring 0)) 
  (ring 1)
 (first (ring 1)) 
  (vec (flatten [(second (ring 1)) 1 (last (ring 1)) (last (ring 2)) (take 2 (ring 2))]))
  3
  }

() (ring 2)

{2 []}

{1 [2 3 4 5 6 7]
 2 [3 1 7 19 8 9]
 3 []}




(defn problem128 [] 0)
