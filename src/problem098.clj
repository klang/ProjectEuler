(ns problem098
  (:use [clojure.set :only (union)]
	[clojure.test :only (deftest is)]
	[clojure.math.numeric-tower :only (expt)]
	[clojure.pprint :only (pprint)]
	[clojure.math.combinatorics :only (combinations)]
	[tools.numbers :only (digits integer)]
	[clojure.string :only (split)])
  (:require [clojure.string :as s :only (replace)]))

(defonce words (map #(s/replace % "\"" "") (split (slurp "src/words.txt") #",")))

(defn word-sort
  "sorting the letters of a word to have a valid key for word classification"
  [w] (apply str (sort (seq w))))

(def word-sorts (map #(hash-map (word-sort %) #{%}) words))
(def sorted-words (apply merge-with union word-sorts))
(def target-words (vals (filter #(< 1 (count (second %))) sorted-words)))

;; (use '[problem061 :only (squares)])
(def squares 
     (map (fn [n] (* n n)) (iterate inc 1)))

(defn possible-squares [digits]
  (take-while  #(< % (expt 10 digits)) (drop-while #(< % (expt 10 (dec digits))) squares)))

(comment
  (sort (keys (frequencies (map count (keys (filter #(< 1 (count (second %))) sorted-words))))))
  ;; => (2 3 4 5 6 8 9)
  ;; produce all squares up to and including 9 digits,
  ;; excluding 1 and 7 digit squares

  ;; watch out for 900 and 400 .. leading zeroes
  )

  ;; match 2 digit squares to 2 digit words

(defn number-sort [num]
  "sorting the letters of a number to have a valid key for number classification"
  (apply str (sort (digits num))))

(def number-sorts (map #(hash-map (number-sort %) #{%})
		       (flatten (map #(possible-squares %) [2 3 4 5 6 8 9]))))

(def sorted-numbers (apply merge-with union number-sorts))
(def target-numbers (vals (filter #(< 1 (count (second %))) sorted-numbers)))

;;(count target-numbers)
;; 6654
;;(count target-words)
;; 42

;; (filter #(= 2 (count (digits (first %)))) target-numbers)
;; ()
;; so, there are no 2 digit numbers to match the two letter words

;; for 3 digit numbers we have a result
;; (filter #(= 3 (count (digits (first %)))) target-numbers)
;; (#{144 441} #{256 625} #{961 196 169})
;; (filter #(= 3 (count (first %))) target-words)
;; (#{"ITS" "SIT"} #{"GOD" "DOG"} #{"HOW" "WHO"} #{"TEA" "EAT"} #{"ACT" "CAT"} #{"OWN" "NOW"})

(comment
  (apply merge-with union (map #(hash-map (count (first %)) %) target-words))
  (apply merge-with union (map #(hash-map (vals (frequencies (first %))) %) target-words))
  ;;
  (apply merge-with merge (map #(hash-map (sort (vals (frequencies (first %)))) %) target-words))
  ;; #{#{"NEAR" "EARN"} "POST" #{"TIME" "ITEM"} #{"USER" "SURE"} "STOP" "SPOT" #{"RACE" "CARE"} ..}
  ;;                     ^                                        ^      ^ not what we want
  ;; we need to make a function to obtain the desired result (frequency-merge-words target-words)

  (apply merge-with merge (map #(hash-map (sort (vals (frequencies (digits (first %))))) %) target-numbers))
  ;; and the same applies to the above form. (frequency-merge-numbers target-numbers) will solve the problem
  )

(comment
  ;; with these two definitions, the loop can be checked
  (def merged {})
  (def words (map #(hash-map (sort (vals (frequencies (first %)))) %) target-words))
  )

(defn frequency-merge
  [seq]
  (loop [merged {} words seq]
    (if (empty? words)
      merged
      (recur (if (nil? (merged (first (keys (first words)))))
	       (merge-with merge merged {(first (keys (first words)))  #{}} (first words))
	       (merge-with merge merged (first words)))
	     (rest words)))))

(def frequency-merge-words
     (frequency-merge (map #(hash-map (sort (vals (frequencies (first %)))) %) target-words)))

(def frequency-merge-numbers
     (frequency-merge (map #(hash-map (sort (vals (frequencies (digits (first %))))) %) target-numbers)))

;; if frequencies of an element of target-numbers matches the frequencies of
;; an element in target-words, then we have a chance to match up the elements,
;; otherwise it will be a bit difficult to make a one-to-one mapping.

(comment
  (contains? #{256 625} (integer (map #((zipmap "ITS" (digits 256)) %) "SIT")))	;;--> true
  (contains? #{256 625} (integer (map #((zipmap "ITS" (digits 625)) %) "SIT")))	;;--> false
  )

;; as the number group contains far less elements (41 + 3 different word pairs)
;; (flatten (map #(map count (second %)) frequency-merge-words))
;; We know there is one set with a tripplet, so we take that one out, and put
;; the 3 different combinations back in.

(def word-groups
     (assoc frequency-merge-words '(1 1 1 1)
	    (union
	     (disj  (frequency-merge-words '(1 1 1 1)) #{"POST" "STOP" "SPOT"} )
	     (map #(hash-set (first %) (second %)) (combinations #{"POST" "STOP" "SPOT"} 2)))))

;; reduce frequency-merge-numbers to relevant frequencies
(def number-groups
  (into {} (map #(hash-map % (frequency-merge-numbers %)) (keys word-groups))))

(defn zip-assign [[word1 word2] number]
  (integer (map #((zipmap word1 (digits number)) %) word2)))

;; for each word-pair,
;;   we fetch the set in frequency-merge-numbers with the corresponding frequency
;;   then we check if zip-assigning the words to the number leads to a number from the group
(comment
  (contains? #{256 625} (zip-assign ["ITS" "SIT"] 256))
  (contains? #{256 625} (zip-assign ["ITS" "SIT"] 625)))
;;   if it does, the word-pair, the assigning number and the zip-assign result is stored
;;   store the current max as well, for good measure.

(defn matches [word-groups number-groups]
  (loop [word-groups word-groups
	 the-max 0]
    (let [word-pairs (first word-groups)
	  word-frequency (first word-pairs)
	  number-groups (number-groups word-frequency)]
      (if (empty? word-groups)
	  the-max
	  (let [maxes (flatten (for [word-pair (second word-pairs) number-group number-groups]
				 (do #_(pprint {:word-pair word-pair
						:number-group number-group})
				     (for [number number-group
					   :let [result (zip-assign (vec word-pair) number)]
					   :when (contains? number-group result)]
				       (do #_(pprint {:number-pair [number result]
						    :word-pair word-pair})
					   (max number result))))))]
	    (recur (rest word-groups)
		   (apply max (flatten (cons the-max  maxes)))))))))

;; problem098> (time (matches word-groups number-groups))
;; "Elapsed time: 221.400845 msecs"
;; 18769
(defn problem098 [] (matches word-groups number-groups))
;;-----------------------------------------------------------------------------
;;-----------------------------------------------------------------------------

