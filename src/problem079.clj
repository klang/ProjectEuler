(ns problem079
  (:use clojure.contrib.duck-streams)
  (:use [clojure.contrib.str-utils2 :only (split)])
  (:use clojure.test)
  (:use tools.numbers))

(def keylog-txt (slurp "keylog.txt") )

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
