(ns problem042
  (meta {:description "
The n-th term of the sequence of triangle numbers is given by, t(n) = ½n(n+1); so the first ten triangle numbers are:

 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t(10). If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?"})
  (:require [clojure.string :only (split replace) :as str]))

(defn triangle [n] (quot (* n (+ n 1)) 2))
(defn triangles [] 
  (map (fn [n] (quot (* n (+ n 1)) 2)) (iterate inc 1)))
(defn triangles []
  (map first (iterate (fn [[a b]] [(+ a b) (inc b)]) [1 2])))

;; user> (char 65)
;; \A
;; (- (int \A) 64)
(defn word-value [w] 
  (reduce + (map #(- (int %) 64) w)))

(def words-txt (slurp "src/words.txt") )
;; (take 5 (map #(str/split % #",") (str/split words-txt #",")))
;; (str/split words-txt #"[\",]+")
;; (drop-last 1 (drop 1 (str/split words-txt #",")))
;; (map #(drop-last 1 (drop 1 %)) (str/split words-txt #","))
;; not practical representation, given word-value

;; (map #(str/replace % "\"" "") (str/split words-txt #","))
;; (map #(word-value %) (map #(str/replace % "\"" "") (str/split words-txt #",")))

;; (reduce #(max %1 %2) (map #(word-value %) (map #(str/replace % "\"" "") (str/split words-txt #","))))
;; 192 is the maximal triangle number needed
;; user> (take-while #(<= % 192) (triangles))
;;(1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190)
(def target-triangles (into #{} (take-while #(<= % 192) (triangles))))
(def word-values (map #(word-value %) (map #(str/replace % "\"" "") (str/split words-txt #","))))
(def triangle-words (filter #(contains? target-triangles %) word-values))
;;user> (count triangle-words)
;;162

(defn problem042 []
  (count triangle-words))
