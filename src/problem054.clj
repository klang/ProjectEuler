(ns problem054
  (meta {:description "evaluate poker hands"
	 :help "http://www.pagat.com/vying/pokerrank.html"})
  (:use clojure.test
	[clojure.contrib.str-utils2 :only (split)]))

(def poker-txt (slurp "src/poker.txt"))

(def suits [:S :H :D :C])
(def ranks [:2 :3 :4 :5 :6 :7 :8 :9 :T :J :Q :K :A])
(def cards (zipmap ranks (iterate inc 2)))

(def input (map #(split % #" ") (split poker-txt #"\r\n")))

(defn parse
  "parses a card given on the short form, returns a card vector"
  [card]
  ;; last entry is always the suit,
  ;; the rest is either 1 or 2 chars long and is the rank
  ;; This parser takes 10 into account (10 is encoded as T in the input)
  [(keyword (apply str (take (- (count card) 1) card))) (keyword (str (last card)))])

(deftest test-parse
  (is (= [:T :C] (parse "TC")))
  (is (= [:7 :C] (parse "7C")))
  (is (= [:K :P] (parse "KP"))))

(def hand-rank
     (zipmap  [:high-card :one-pair :two-pairs :three-of-a-kind :straight
	       :flush :full-house :four-of-a-kind :straight-flush :royal-flush] (iterate inc 1)))

(defn straight? [hand-input]
  (some
   (fn [straight] (= (set (map first hand-input)) (set straight)))
   (partition 5 1 [:A :2 :3 :4 :5 :6 :7 :8 :9 :T :J :Q :K :A])))

(defn flush? [hand-input]
  (= 1 (count (distinct (map second hand-input)))))

(defn straight-flush? [hand-input]
  (and (straight? hand-input) (flush? hand-input)))

(defn royal-flush? [hand-input]
  (and (= (set (map first hand-input)) (set [:T :J :Q :K :A]))
       (flush? hand-input)))

(defn full-house? [hand-input]
  (= (set [3 2])
     (set (vals (frequencies (map first hand-input))))))

(defn one-pair? [hand-input]
  (some (partial = 2)
	(vals (frequencies (map first hand-input)))))

(defn two-pairs? [hand-input]
  (= '(1 2 2)
     (sort (vals (frequencies (map first hand-input))))))

(defn three-of-a-kind? [hand-input]
  (some (partial = 3)
	(vals (frequencies (map first hand-input)))))

(defn four-of-a-kind? [hand-input]
  (some (partial = 4)
	(vals (frequencies (map first hand-input)))))

(defn hand-type [hand-input]
  (cond (royal-flush? hand-input)     :royal-flush
	(straight-flush? hand-input)  :straight-flush
	(four-of-a-kind? hand-input)  :four-of-a-kind
	(full-house? hand-input)      :full-house
	(flush? hand-input)           :flush
	(straight? hand-input)        :straight
	(three-of-a-kind? hand-input) :three-of-a-kind
	(two-pairs? hand-input)       :two-pairs
	(one-pair? hand-input)        :one-pair
	:else                         :high-card))

(defn hand-and-strength [hand-input]
	   (let [strength (into (sorted-map) (frequencies (map cards (map first hand-input))))]
	     [(hand-type hand-input) strength]))

;; a different way to write the two functions above

(defn hand-and-strength [hand-input]
  (let [card-ranks      (map first hand-input)
	values          (vals (frequencies card-ranks))
	strength        (into (sorted-map) (frequencies (map cards card-ranks)))
 	straight?        (some
 			  (fn [straight] (= (set card-ranks) (set straight)))
 			  (partition 5 1 [:A :2 :3 :4 :5 :6 :7 :8 :9 :T :J :Q :K :A]))
 	flush?           (= 1 (count (distinct (map second hand-input))))
 	straight-flush?  (and flush? straight? )
 	royal-flush?     (and flush? (= (set card-ranks) (set [:T :J :Q :K :A])))
 	full-house?      (= '(2 3) (sort values))
 	one-pair?        (some (partial = 2) values)
 	two-pairs?       (= '(1 2 2) (sort values))
 	three-of-a-kind? (some (partial = 3) values)
 	four-of-a-kind?  (some (partial = 4) values)
 	hand-type        (cond royal-flush?     :royal-flush
			       straight-flush?  :straight-flush
 			       four-of-a-kind?  :four-of-a-kind
 			       full-house?      :full-house
 			       flush?           :flush
 			       straight?        :straight
 			       three-of-a-kind? :three-of-a-kind
 			       two-pairs?       :two-pairs
 			       one-pair?        :one-pair
 			       :else            :high-card)]
    [hand-type strength]))

(defn strength
  "the rules as stated http://www.pagat.com/vying/pokerrank.html have been implemented"
  ([hinput]
     (let [hstrength (into (sorted-map)
			  (frequencies (map cards (map first hinput))))]
       (strength (hand-type hinput) hstrength)))
  ([htype strength]
     (let [highest (first (last strength))
	   lowest  (first (first strength))]
       (cond (= htype :royal-flush)     {:hand [] :rest []} ;; will always tie
	     (= htype :straight-flush ) {:hand [(+ 4 lowest)] :rest []} ;; OBS: A 2 3 4 5
	     (= htype :four-of-a-kind ) (let [hand (first (first (filter #(= 4 (val %)) strength)))]
					  {:hand [hand]
					   :rest (vec (keys (dissoc strength hand)))})
	     (= htype :full-house     ) {:hand [(first (first (filter #(= 3 (val %)) strength)))]
					 :rest [(first (first (filter #(= 2 (val %)) strength)))]}
	     (= htype :flush          ) {:hand [highest]
					 :rest (vec (reverse (keys (dissoc strength highest))))}
	     (= htype :straight       ) {:hand [(+ 4 lowest)] :rest []} ;; OBS: A 2 3 4 5
	     (= htype :three-of-a-kind) (let [hand (first (first (filter #(= 3 (val %)) strength)))]
					  {:hand [hand]
					   :rest (vec (reverse (keys (dissoc strength hand))))})
	     (= htype :two-pairs      ) (let [hand (vec (map first (filter #(= 2 (val %)) strength)))]
					  {:hand hand
					   :rest (vec (keys (filter #(= 1 (val %)) strength)))})
	     (= htype :one-pair       ) (let [hand (first (first (filter #(= 2 (val %)) strength)))]
					  {:hand [hand]
					   :rest (vec (reverse (keys (dissoc strength hand))))})
	     (= htype :high-card      ) {:hand [highest]
					 :rest (vec (reverse (keys (dissoc strength highest))))}))))

(def royal-flush-H       (set (map parse ["TH" "JH" "QH" "KH" "AH"])))
(def royal-flush-S       (set (map parse ["TS" "JS" "QS" "KS" "AS"])))
(def straight-flush-6    (set (map parse ["6H" "2H" "3H" "4H" "5H"])))
(def straight-flush-6S   (set (map parse ["6S" "2S" "3S" "4S" "5S"])))
(def straight-flush-7    (set (map parse ["6H" "7H" "3H" "4H" "5H"])))
(def four-of-a-kind-T   (set (map parse ["TH" "TC" "TS" "TD" "3H"])))
(def four-of-a-kind-A    (set (map parse ["AH" "AC" "AS" "AD" "2H"])))


(def one-pair-T-QKA     (set (map parse ["TH" "TC" "QH" "KH" "AH"])))
(def one-pair-T-9KA     (set (map parse ["TH" "TC" "9H" "KH" "AH"])))
(def one-pair-T-8KA     (set (map parse ["TH" "TC" "8H" "KH" "AH"])))
(def two-pairs-8         (set (map parse ["TH" "TC" "9H" "9C" "8H"])))
(def two-pairs-K         (set (map parse ["TH" "TC" "9H" "9C" "KH"])))
(def two-pairs-A         (set (map parse ["TH" "TC" "9H" "9C" "AH"])))
(def straight-6          (set (map parse ["6H" "2H" "3C" "4S" "5D"])))
(def straight-7          (set (map parse ["7H" "3H" "4C" "5S" "6D"])))
(def straight-9          (set (map parse ["9H" "5H" "6C" "7S" "8D"])))
(def flush-7             (set (map parse ["7H" "2H" "3H" "4H" "5H"])))
(def flush-8             (set (map parse ["8H" "2H" "3H" "4H" "5H"])))
(def flush-9             (set (map parse ["9H" "2H" "3H" "4H" "5H"])))

(deftest type-strength
  ;; max compare values of :hand, then compare :rest from top to bottom
  (is (= {:hand [], :rest []} (strength royal-flush-H)))
  (is (= {:hand [], :rest []} (strength royal-flush-S)))
  (is (= {:hand [6], :rest []} (strength straight-flush-6) (strength straight-flush-6S)))
  (is (= {:hand [7], :rest []} (strength straight-flush-7)))
  (is (= {:hand [10], :rest [3]} (strength four-of-a-kind-T)))  
  (is (= {:hand [14], :rest [2]} (strength four-of-a-kind-A)))  
  (is (= {:hand [10] :rest [14 13 12]} (strength one-pair-T-QKA)))
  (is (= {:hand [10] :rest [14 13 9]}  (strength one-pair-T-9KA)))
  (is (= {:hand [10] :rest [14 13 8]}  (strength one-pair-T-8KA)))
  (is (= {:hand [9 10] :rest [14]}     (strength two-pairs-A)))
  (is (= {:hand [9 10] :rest [13]}     (strength two-pairs-K)))
  (is (= {:hand [9 10] :rest [8]}      (strength two-pairs-8)))
  (is (= {:hand [7] :rest []} (strength straight-7)))
  (is (= {:hand [9] :rest []} (strength straight-9)))
  (is (= {:hand [6] :rest []} (strength straight-6))))

(defn hand [hand-input]
  (let [data (hand-and-strength hand-input)]
    [(first data) (strength data)]))

(def straight            (set (map parse ["6H" "2H" "3C" "4S" "5D"])))
(def flush-8             (set (map parse ["8H" "2H" "3H" "4H" "5H"])))
(def straight-flush      (set (map parse ["6H" "2H" "3H" "4H" "5H"])))
(def royal-flush         (set (map parse ["TH" "JH" "QH" "KH" "AH"])))
(def one-pair            (set (map parse ["TH" "TC" "8H" "KH" "AH"])))
(def two-pairs           (set (map parse ["TH" "TC" "9H" "9C" "8H"])))
(def three-of-a-kind     (set (map parse ["TH" "TC" "TS" "9D" "AH"])))
(def four-of-a-kind      (set (map parse ["TH" "TC" "TS" "TD" "AH"])))
(def full-house-4        (set (map parse ["2H" "2D" "4C" "4D" "4S"])))
(def full-house-3        (set (map parse ["3C" "3D" "3S" "9S" "9D"])))
(def full-house-A        (set (map parse ["AC" "AD" "AS" "KS" "KD"])))
(def high-card           (set (map parse ["4C" "2D" "3S" "7S" "8D"])))

(def pair-of-fives       (set (map parse ["5H" "5C" "6S" "7S" "KD"])))
(def pair-of-eights      (set (map parse ["2C" "3S" "8S" "8D" "TD"])))
(def highest-card-ace    (set (map parse ["5D" "8C" "9S" "JS" "AC"])))	 	
(def highest-card-queen  (set (map parse ["2C" "5C" "7D" "8S" "QH"])))
(def three-aces          (set (map parse ["2D" "9C" "AS" "AH" "AC"])))	 	
(def flush-with-diamonds (set (map parse ["3D" "6D" "7D" "TD" "QD"])))
(def pair-of-queens-9    (set (map parse ["4D" "6S" "9H" "QH" "QC"])))
(def pair-of-queens-7    (set (map parse ["3D" "6D" "7H" "QD" "QS"])))

(deftest test-hand
  (is (= (map hand-type
	      [one-pair two-pairs three-of-a-kind straight flush-8
	       full-house-A full-house-3 full-house-4 four-of-a-kind
	       straight-flush royal-flush])
	 '(:one-pair :two-pairs :three-of-a-kind :straight :flush
		     :full-house :full-house :full-house :four-of-a-kind
		     :straight-flush :royal-flush)))
  (is (= (map hand-type
	      [pair-of-fives    pair-of-eights      
	       highest-card-ace highest-card-queen  
	       three-aces       flush-with-diamonds 
	       pair-of-queens-9 pair-of-queens-7])
	 '(:one-pair :one-pair
		     :high-card :high-card
		     :three-of-a-kind :flush
		     :one-pair :one-pair))))

(defn tie-hands [hand1 hand2]
  (let [st1 (strength hand1)
	st2 (strength hand2)]
    (cond (some true? (map < (:hand st1) (:hand st2))) :player2
	  (some true? (map > (:hand st1) (:hand st2))) :player1
	  (some true? (map < (:rest st1) (:rest st2))) :player2
	  (some true? (map > (:rest st1) (:rest st2))) :player1
	  :else :tie)))

(deftest test-tie-hands
  (is (= :tie (tie-hands royal-flush-H royal-flush-S)))
  (is (= :tie (tie-hands straight-flush-6 straight-flush-6S)))
  (is (= :player1 (tie-hands straight-flush-7 straight-flush-6)))
  (is (= :player1 (tie-hands four-of-a-kind-A four-of-a-kind-T)))
  (is (= :player1 (tie-hands one-pair-T-QKA one-pair-T-9KA)))
  (is (= :player1 (tie-hands one-pair-T-9KA one-pair-T-8KA)))
  (is (= :player1 (tie-hands two-pairs-K two-pairs-8)))
  (is (= :player1 (tie-hands two-pairs-A two-pairs-K)))
  (is (= :player1 (tie-hands straight-7 straight-6)))
  (is (= :player1 (tie-hands straight-9 straight-7)))
  (is (= :player1 (tie-hands flush-8 flush-7)))
  (is (= :player1 (tie-hands full-house-A full-house-3)))
  (is (= :tie (tie-hands high-card high-card)))
  (is (= :tie (tie-hands pair-of-queens-9 pair-of-queens-9)))
  (is (= :player1 (tie-hands pair-of-queens-9 pair-of-queens-7))))

(defn winner [hand1 hand2]
  (let [hr1 (hand-rank (hand-type hand1))
	hr2 (hand-rank (hand-type hand2))]
    (cond (< hr1 hr2) :player2
	  (> hr1 hr2) :player1
	  :else (tie-hands hand1 hand2))))

(defn find-winner [player1 player2]
  (let [hand1 (set (map parse player1))
	hand2 (set (map parse player2))]
    (winner hand1 hand2)))

(deftest test-winner
    (is (= :player2 (tie-hands pair-of-fives pair-of-eights)))
    (is (= :player1 (tie-hands highest-card-ace highest-card-queen)))
    (is (= :player2 (find-winner three-aces flush-with-diamonds)))
    (is (= :player1 (tie-hands pair-of-queens-9  pair-of-queens-7)))
    (is (= :player1 (tie-hands full-house-4 full-house-3))))



(deftest test-find-winner
  (is (= :player1 (winner pair-of-queens-9 pair-of-queens-7)))
  (is (= :player1 (find-winner ["4D" "6S" "9H" "QH" "QC"] ["3D" "6D" "7H" "QD" "QS"]))))

(defn problem054 []
  (count
   (for [game input
	 [player1 player2] [(partition 5 game)]
	 :when (= :player1 (find-winner player1 player2))]
     :player1)))

;;problem054> (time (problem054))
;;"Elapsed time: 2325.333083 msecs"
;;376

;;(run-tests)