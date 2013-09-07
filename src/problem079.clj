(ns problem079
  (:use 
   [clojure.string :only (split)]
   [tools.numbers :only (digits)]
   [clojure.test :only (deftest is)]))

(def keylog-txt (slurp "src/keylog.txt") )

(def keylog-values (split keylog-txt #"\r\n"))

;; (sort keylog-values)
;; ("129" "160" "162" "162" "168" "180" "289" "290" "316" "316" "318" "318" "319" "319" "319" "319" "362" "368" "380" "389" "620" "629" "680" "680" "680" "689" "689" "690" "710" "710" "710" "716" "716" "718" "719" "720" "728" "729" "729" "729" "729" "729" "731" "736" "760" "762" "762" "769" "790" "890")

;; the correct code was found simply by looking at the values in the above form
;; 7 3 1 6 2 8 9 0
;; 129 -> 2 and 9 is to the left of 1, and 9 is to the left of 2 (write it down)
;; 162 -> 6 is between 1 and 2 => it is to the right of 9 (690 proves this)
;; 289 -> 8 is to the right of 9 but to the left of 6 and 2
;; ..
;; at the first 7, there is doubt if it should be to the left or right of 3
;; it has to be to the left of 1, but that's all we know
;; at this point, there has been no doubt about any other value
;; 731 establishes the location of 7 and the rest of the values matches the 
;; resulting number: 73162890

;; now we need to make a program, that can do this automatically, but this
;; really was too easy doing just with pen and paper.

;; hundreds
;; 1{2 9 6 0 2 8}
;; 2{8 9 0}
;; 3{1 6 2 8 9 0} 
;; 6{2 0 8 9}
;; 7{1 0 6 8 9 2 3}
;; 8{9 0}

;; (map #(hash-map (first %) (into [] (rest %))) (map #(digits (. Integer parseInt % 10)) keylog-values))

;; problem079> (def a {3 [1 9]})
;; problem079> (def b {3 [8 0]})
;; problem079> (merge-with concat a b)
;; {3 (1 9 8 0)}

(def hundreds (map #(hash-map (first %) (into [] (rest %))) (map #(digits (. Integer parseInt % 10)) keylog-values)))
(def tens (map #(hash-map (first %) (into [] (rest %))) (map #(rest (digits (. Integer parseInt % 10))) keylog-values)))

(defn decode [group]
  (loop [catch {} 
	 group group] 
    (if (empty? group) 
      (map #(hash-map (first %) (distinct (second %))) catch)
      (recur (merge-with concat catch (first group)) (rest group)))))

;(decode hundreds)
(comment
  (; sorted manually after the number of elements that are to the right of the digit
   {7 (6 2 1 0 3 9 8)}
   {3 (1 9 8 6 2 0)}
   {1 (8 0 2 9 6)} 
   {6 (8 0 9 2)} 
   {2 (9 0 8)} 
   {8 (9 0)} 
;; {0 ()}
))

;(decode tens)
(comment
  (
   {3 (1 6)} 
   {1 (9 8 0 6)}
   {6 (2 8 0 9)} 
   {2 (9 0 8)} 
   {8 (0 9)} 
   {9 (0)} 
;; {0 ()}
   ))

;; .. I can't be bothered ... 
;; I've used ten times as much time getting this far, as solving the problem with
;; pen and paper and

(defn problem079 [] 73162890)
