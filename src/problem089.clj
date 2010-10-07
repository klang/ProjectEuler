(ns problem089
  (:use [clojure.contrib.str-utils2 :only (split)])
  (:use [clojure.contrib.string :only (replace-re)])
  (:use [tools.numbers :only (digits)])
  (:use clojure.test))

(def roman-txt (slurp "src/roman.txt") )

(def roman-values (split roman-txt #"\r\n"))

;;Only I, X, and C can be used as the leading numeral in part of a subtractive pair.
;; I can only be placed before V and X.
;; X can only be placed before L and C.
;; C can only be placed before D and M.
(defn reduce-I [str] (replace-re #"IIIII" "V" str))
(defn reduce-V [str] (replace-re #"VV" "X" str))
(defn reduce-X [str] (replace-re #"XXXXX" "L" str))
(defn reduce-L [str] (replace-re #"LL" "C" str))
(defn reduce-C [str] (replace-re #"CCCCC" "D" str))
(defn reduce-D [str] (replace-re #"DD" "M" str))

(defn rewrite-9 [str] (replace-re #"VIV" "IX" str))
(defn rewrite-90 [str] (replace-re #"LXL" "XC" str))
(defn rewrite-900 [str] (replace-re #"DCD" "CM" str))
(defn rewrite-I [str] (replace-re #"IIII" "IV" str))
(defn rewrite-X [str] (replace-re #"XXXX" "XL" str))
(defn rewrite-C [str] (replace-re #"CCCC" "CD" str))

(defn reduce-numeral [str]
  (-> str
      (reduce-I) (reduce-V) (reduce-X) (reduce-L) (reduce-C) (reduce-D)
      (rewrite-I)(rewrite-X) (rewrite-C) (rewrite-9)(rewrite-90)(rewrite-900)))

(deftest test-reduce-numeral
  (is (= (reduce-numeral "IIIIIIIIIIIIIIII")
	 "XVI"))
  (is (= (reduce-numeral "MCCCCCCVI")
	 "MDCVI"))
  (is (every? #(= % "XVI")  ; 16
	      (map #(reduce-numeral %) ["XVI" "XIIIIII" "VVVI"])))
  (is (every? #(= % "XIX")  ; 19
	      (map #(reduce-numeral %) ["XIX" "XVIIII" "XVIV"])))
  (is (every? #(= % "XLIX") ; 49
	      (map #(reduce-numeral %) ["XXXXVIIII" "XXXXIX" "XLVIIII" "XLIX"]))))

(def roman-values-chars (reduce + (map #(count %) roman-values)))
(def reduced-roman-values-chars (reduce + (map #(count (reduce-numeral %)) roman-values)))

(defn problem089 [] (- roman-values-chars reduced-roman-values-chars))