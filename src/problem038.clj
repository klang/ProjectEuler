(load "tools")
(use 'clojure.contrib.combinatorics)

(count  (permutations '(1 2 3 4 5 6 7 8 9)))

;; find a permutation, p that can be split in n pieces, where
;; (= (reduce + (* n piece-n)) p)

