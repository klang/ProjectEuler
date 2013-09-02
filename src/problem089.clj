(ns problem089
  (:refer-clojure :exclude (replace))
  (:use 
   [clojure.test :only (deftest is)]
   [clojure.string :only (split replace)]))

(def roman-txt (slurp "src/roman.txt") )

(def roman-values (split roman-txt #"\r\n"))

;;Only I, X, and C can be used as the leading numeral in part of a subtractive pair.
;; I can only be placed before V and X.
;; X can only be placed before L and C.
;; C can only be placed before D and M.
(defn reduce-I [str] (replace #"IIIII" "V" str))
(defn reduce-V [str] (replace #"VV" "X" str))
(defn reduce-X [str] (replace #"XXXXX" "L" str))
(defn reduce-L [str] (replace #"LL" "C" str))
(defn reduce-C [str] (replace #"CCCCC" "D" str))
(defn reduce-D [str] (replace #"DD" "M" str))

(defn rewrite-9 [str] (replace #"VIV" "IX" str))
(defn rewrite-90 [str] (replace #"LXL" "XC" str))
(defn rewrite-900 [str] (replace #"DCD" "CM" str))
(defn rewrite-I [str] (replace #"IIII" "IV" str))
(defn rewrite-X [str] (replace #"XXXX" "XL" str))
(defn rewrite-C [str] (replace #"CCCC" "CD" str))

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
