(ns problem024
  (:require [clojure.math.combinatorics :only (lex-permutations) :as comb]))

;; user> (lex-permutations [0 1 2])
;; ([0 1 2] [0 2 1] [1 0 2] [1 2 0] [2 0 1] [2 1 0])

;; user> (nth (lex-permutations [0 1 2]) 5)
;; [2 1 0]
(defn problem024 []
  (. Long parseLong (apply str (nth (comb/lex-permutations [0 1 2 3 4 5 6 7 8 9]) 999999))))

;; user> (time (nth (comb/lex-permutations [0 1 2 3 4 5 6 7 8 9]) 999999))
;; "Elapsed time: 5238.988329 msecs"
;; [2 7 8 3 9 1 5 4 6 0]

