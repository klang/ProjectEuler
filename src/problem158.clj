(ns problem158
  (meta
   {:description "Taking three different letters from the 26 letters of the alphabet, character strings of length three can be formed.
Examples are 'abc', 'hat' and 'zyx'.
When we study these three examples we see that for 'abc' two characters come lexicographically after its neighbour to the left.
For 'hat' there is exactly one character that comes lexicographically after its neighbour to the left. For 'zyx' there are zero characters that come lexicographically after its neighbour to the left.
In all there are 10400 strings of length 3 for which exactly one character comes lexicographically after its neighbour to the left.

We now consider strings of n  26 different characters from the alphabet.
For every n, p(n) is the number of strings of length n for which exactly one character comes lexicographically after its neighbour to the left.

What is the maximum value of p(n)?"})
  (:use [clojure.contrib.combinatorics :only (combinations permutations selections)])
  (:use [clojure.contrib.math :only (expt)])
  (:use clojure.test))

(comment
  (count (filter true? (map #(< (first %) (second %)) (partition 2 1 [99] (map int '(\a \b \c))))))
  (count (filter true? (map #(< (first %) (second %)) (partition 2 1 [116] (map int '(\h \a \t))))))
  (count (filter true? (map #(< (first %) (second %)) (partition 2 1 [120] (map int '(\z \y \x)))))))

(defn one?
  [coll]
  (= 1
     (count
      (filter
       true?
       (map #(< (first %) (second %))
	    (partition 2 1 (vector (int (last coll))) (map int coll)))))))

(deftest test-one?
  (is (not (one? (seq "abc"))))
  (is (one? (seq "hat")))
  (is (not (one? (seq "zyx")))))

;;(selections (seq "abcdefghijklmnopqrstuvwxyz") 3)
;; 17576 ways
;;(combinations (seq "abcdefghijklmnopqrstuvwxyz") 3)
;; 2600 ways
;; for each combination, there are 6 permutations
;; all in all 15600 ways

(defn count-ones [] (combinations (seq "abcdefghijklmnopqrstuvwxyz") 3))

(comment
  (def g (take 2 (combinations (seq "abcdefghijklmnopqrstuvwxyz") 3)))
  (reduce into [] (map permutations g))
  (count (filter one? (reduce into []
			      (map permutations
				   (combinations (seq "abcdefghijklmnopqrstuvwxyz") 3)))))
  ;;10400
  )

(defn p
  "is the number of strings of length n for which exactly one character comes lexicographically after its neighbour to the left."
  [n]
  (count (filter one? (reduce into []
			      (map permutations
				   (combinations (seq "abcdefghijklmnopqrstuvwxyz") n))))))
;; problem158> (time (p 2))
;; "Elapsed time: 38.62727 msecs"
;; 325
;; problem158> (time  (p 3))
;; "Elapsed time: 542.008736 msecs"
;; 10400
;; problem158> (time (p 4))
;; out of memory.

;; .. 3
;; 2600 combinations   n! / (n-k)! k!
;; (/ (factorial 26) (* (factorial (- 26 3)) (factorial 3)))
;; (* 3 2 1) 6 permutations for each combination
;; 2 out of 3 of these permutations will be of the desired form (in this case)
;; (* 2/3 (* 2600 (* 3 2 1)))

(defn factorial [n] 
  (reduce * (range n 0 -1)))

(defn combinations#
  "returns the number of combinations"
  [n k]
  (/ (factorial n) (* (factorial (- n k)) (factorial k))))

(defn permutations#
  "returns the number of permutations" 
  [n]
  (factorial n))

;; case p(2)
;; 325 combinations, 2 permutations of each, one of them is on the right form => 325

;; case p(4)

(comment
  (def g (take 1 (combinations (seq "abcdefghijklmnopqrstuvwxyz") 4)))
  (reduce into [] (map permutations g))
  ;; 24
  (map #(apply str %) (reduce into [] (map permutations g)))
  ;; ("abcd" "abdc" "acbd" "acdb" "adbc" "adcb" "bacd" "badc" "bcad" "bcda" "bdac" "bdca" "cabd" "cadb" "cbad" "cbda" "cdab" "cdba" "dabc" "dacb" "dbac" "dbca" "dcab" "dcba")

  (count (filter one? (reduce into [] (map permutations g))))
  ;; 11
  (map #(apply str %) (filter one? (reduce into [] (map permutations g))))
  ;; ("adcb" "badc" "bdca" "cadb" "cbad" "cbda" "cdba" "dacb" "dbac" "dbca" "dcab")
  )

(defn ones [n]
  (count (filter one?
		 (reduce into []
			 (map permutations
			      (take 1 (combinations (seq "abcdefghijklmnopqrstuvwxyz") n)))))))
(comment
  (map ones (range 1 9))
  ;;(0 1 4 11 26 57 120 247)
  ;; might be http://www.research.att.com/~njas/sequences/A000295 (eulerian numbers)
  ;; 0,1,4,11,26,57,120,247,502,1013,2036,4083,8178,16369,32752,65519,131054,262125,524268,1048555,2097130,4194281,8388584,16777191,33554406,67108837  
  )

(defn eulerian-numbers [n] (- (expt 2 n) n 1))


(defn pp
  "is the number of strings of length n for which exactly one character comes lexicographically after its neighbour to the left."
  [n]
  (let [ps (permutations# n)
	cs (combinations# 26 n)
	ones (eulerian-numbers n)]
    (* (* ps cs) (/ ones ps))))

;; (reduce + (map pp (range 1 27)))
;; 2540926304233
;; not correct, because it wasn't the sum that was asked for, but the maximum value

;; (reduce max (map pp (range 1 27)))
;; 409511334375

(defn problem158 [] (reduce max (map pp (range 1 27))))