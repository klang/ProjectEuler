(ns problem162
  (:use clojure.contrib.combinatorics
	tools.numbers
	clojure.test))

;(count (filter #(< 1000 %) (map integer (permutations [0 1 2 3]))))
;(count (filter #(< 10000 %) (map integer (permutations [0 1 2 3 4]))))

(defn each-number-presented-once [n]
  (- (factorial n) (factorial (- n 1))))

;;(. Long toHexString (each-number-presented-once 16))
;;"11d6fffe2800"
;;(. (. Long toHexString (each-number-presented-once 16)) toUpperCase)
;;"11D6FFFE2800"

;; A, 0, 1 represented at least once.
;; 16 digit hexadecimal numbers
;; no leading zeroes

; (selections [0 1 2 3 4 5 6 7 8 9 \A \B \C \D \E \F] 16)