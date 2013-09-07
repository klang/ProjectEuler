(ns problem061
  ;;(:use tools.numbers)
  ;;(:use problem045)
  ;; gives lazy sequences:
  ;;  triangles, pentagonals, hexagonals
  ;; and functions:
  ;;  triangle, pentagonal, hexagonal
  ;;(:use clojure.contrib.combinatorics clojure.set clojure.test)  
  (:use 
   [problem045 :only (triangles pentagonals hexagonals triangle pentagonal hexagonal)]
   [clojure.test :only (deftest is)]
   [clojure.set :only (union difference)]
   [clojure.math.combinatorics :only (cartesian-product) :as comb]))

;; lazy sequences
(def squares 
  (map (fn [n] (* n n)) (iterate inc 1)))
;; with this wrong version of heptagonals, one unique 6 element
;; cycle is ALSO produced .. by blind luck/chance (of course it's wrong)
(def heptagonals 
     (map (fn [n] (quot (* n (- (* 5 n) 1)) 3)) (iterate inc 1)))
(def heptagonals 
  (map (fn [n] (quot (* n (- (* 5 n) 3)) 2)) (iterate inc 1)))
(def octagonals 
  (map (fn [n] (* n (- (* 3 n) 2))) (iterate inc 1)))

(deftest test-sequences
  (is (= (take 5 triangles) [1, 3, 6, 10, 15]))
  (is (= (take 5 squares) [1, 4, 9, 16, 25]))
  (is (= (take 5 pentagonals) [1, 5, 12, 22, 35]))
  (is (= (take 5 hexagonals) [1, 6, 15, 28, 45]))
  (is (= (take 5 heptagonals) [1, 7, 18, 34, 55]))
  (is (= (take 5 octagonals) [1, 8, 21, 40, 65])))



;; as functions
(defn square [n] (* n n))
(defn heptagonal [n] (quot (* n (- (* 5 n) 3)) 2))
(defn octagonal [n] (* n (- (* 3 n) 2)))

(def tria (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) triangles))))
(def squa (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) squares))))
(def pent (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) pentagonals))))
(def hexa (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) hexagonals))))
(def hept (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) heptagonals))))
(def octa (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) octagonals))))

(comment
  ;; there are not that many 4 digit numbers
  (map #(count %) [tria squa pent hexa hept octa])
  ;; let's see how the different sets intersect .. not that many common elements
  (map #(count %) [(intersection (set tria) (set squa)) ;; 1225
		   (intersection (set tria) (set pent))
		   (intersection (set tria) (set hexa)) ;; hexa
		   (intersection (set tria) (set hept)) ;; 2926
		   (intersection (set tria) (set octa))

		   (intersection (set squa) (set pent)) ;; 9801
		   (intersection (set squa) (set hexa)) ;; 1225
		   (intersection (set squa) (set hept)) ;; 4489
		   (intersection (set squa) (set octa))

		   (intersection (set pent) (set hexa))
		   (intersection (set pent) (set hept))
		   (intersection (set pent) (set octa))

		   (intersection (set hexa) (set hept))
		   (intersection (set hexa) (set octa))

		   (intersection (set hept) (set octa)) ;; 5208
		   ])
  
)

(comment
  ;; for each element (octa is the smallest group) check two different groups for fronts and backs
  ;; .. a lot of different combinations .. 
  (map #(hash-map :front (quot % 100) :back (rem % 100)) octa)
  )

;; (map #(count %) [tria squa pent hexa hept octa])
;; (96 68 56 48 53 40)

;; we do not have to consider numbers that have a zero in the second position,
;; as there are no 3 digit numbers to match with
;; ab0c 0cde 

(def tria (into #{} (filter #(< 9 (rem % 100)) (take-while #(< % 10000) (drop-while #(<= % 1000) triangles)))))
(def squa (into #{} (filter #(< 9 (rem % 100)) (take-while #(< % 10000) (drop-while #(<= % 1000) squares)))))
(def pent (into #{} (filter #(< 9 (rem % 100)) (take-while #(< % 10000) (drop-while #(<= % 1000) pentagonals)))))
(def hexa (into #{} (filter #(< 9 (rem % 100)) (take-while #(< % 10000) (drop-while #(<= % 1000) hexagonals)))))
(def hept (into #{} (filter #(< 9 (rem % 100)) (take-while #(< % 10000) (drop-while #(<= % 1000) heptagonals)))))
(def octa (into #{} (filter #(< 9 (rem % 100)) (take-while #(< % 10000) (drop-while #(<= % 1000) octagonals)))))

;; (map #(count %) [tria squa pent hexa hept octa])
;; (88 53 47 44 47 30)

;; let's slam some types on the different elements and make a set of everything.
;; That way it's easier to take out an element and filter out all elements of the
;; same type, with the desired :front and :back
(def triam (map #(hash-map :type :tria :front (quot % 100) :back (rem % 100)) tria))
(def squam (map #(hash-map :type :squa :front (quot % 100) :back (rem % 100)) squa))
(def pentm (map #(hash-map :type :pent :front (quot % 100) :back (rem % 100)) pent))
(def hexam (map #(hash-map :type :hexa :front (quot % 100) :back (rem % 100)) hexa))
(def heptm (map #(hash-map :type :hept :front (quot % 100) :back (rem % 100)) hept))
(def octam (map #(hash-map :type :octa :front (quot % 100) :back (rem % 100)) octa))

(def all (union triam squam pentm hexam heptm octam))
;; (filter #(not (= (:type (first all)) (:type %))) all)

(defn not-of-same-type-as
  "retuns the part of the group that that has a different :type than the element"
  [element group]
  (filter #(not (= (:type element) (:type %))) group))

(defn back-matches
  "finds all the elements of different type, that has a :front that matches the :back of the element given"
  [elem group]
  (filter #(= (:back elem) (:front %)) (not-of-same-type-as elem group)))

(defn front-matches
  "finds all the elements of different type, that has a :back that matches the :front of the element given"
  [elem group]
  (filter #(= (:front elem) (:back %)) (not-of-same-type-as elem group)))

;; returns back elements that are not in the same group as front and middle elements
(comment
  (flatten (map #(back-matches (first all)
			       (not-of-same-type-as %
						    (not-of-same-type-as (first all) all)))
		(front-matches (first all) all))))
(defn chain?
  "given a middle element, returns true if it can form part of a back-middle-front chain"
  [elem grp]
  (not (empty? (flatten (map #(back-matches elem
					    (not-of-same-type-as % (not-of-same-type-as elem grp)))
			     (front-matches elem grp))))))

;;in the smallest group, only 22 elements can form part of a chain
(comment
  (count all)
  ;;309
  (count (filter #(chain? % all) octam))
  ;;22
  (count (map #(hash-map :back (back-match % all)
			 :middle %
			 :front (front-match % all))
	      (filter #(chain? % all) octam)))
  ;; 22
  (count (filter #(chain? % all) all))
  ;;225
  )

(defn chain [group]
  (map #(hash-map :back (back-matches % all)
		  :middle %
		  :front (front-matches % all))
       (filter #(chain? % all) group)))

;; (map #(count (chain %)) [triam squam pentm hexam heptm octam])
;; (67 37 33 38 28 22)
;; octam is still the set that has the least elements
(def chains (chain octam))
;;
;;(cartesian-product (:front (first chains)) (list (:middle (first chains))) (:back (first chains)))

(def all-chains (map  #(cartesian-product (:front %) (list (:middle %)) (:back %)) chains))

;; (contains? (into #{} (map #(:type %) foo)) :hexa)

(defn not-of-same-type-as-chain
  "retuns the part of the group that that has a different :type than the element"
  [first-middle-back-chain group]
  (filter (fn [element] (not (contains? (into #{} (map #(:type %) first-middle-back-chain)) (:type element)))) group))

(defn fits-chain-back-and-front
  [first-middle-back-chain group]
  (let [others (not-of-same-type-as-chain first-middle-back-chain group)
	back-must-match (:front (first first-middle-back-chain))
	front-must-match (:back (last first-middle-back-chain))
	candidate (hash-map :front (filter #(= (:back %) back-must-match) others)
			    :middle first-middle-back-chain
			    :back (filter #(= (:front %) front-must-match) others))]
    (if (and (not-empty (:front candidate))
	     (not-empty (:back candidate)))
      candidate
      nil)))

(comment
  (fits-chain-back-and-front (first (apply union all-chains)) all)

  (def chain5  (filter #(not (nil? %)) (map #(fits-chain-back-and-front % all) (apply union all-chains))))
  (count chain5)
  ;; 172

  ;;problem061> (first chain5)
  ;; {:front ({:front 10, :back 33, :type :hept}),
  ;;  :middle ({:front 33, :back 21, :type :tria} {:front 21, :back 33, :type :octa} {:front 33, :back 21, :type :tria}),
  ;;  :back ({:front 21, :back 48, :type :hept}
  ;; 	{:front 21, :back 45, :type :hexa}
  ;; 	{:front 21, :back 47, :type :pent}
  ;; 	{:front 21, :back 16, :type :squa})}
  ;; this is wrong, as the middle elemnet has two :tria's
)

;; not all the chains in the cartesian product of chains fulfill the condition that the individual
;; elements are of different type
(def all-3-chains (filter (fn [elem] (= 3 (count (into #{} (map #(:type %) elem)))))
			  (apply union (map  #(cartesian-product (:front %) (list (:middle %)) (:back %)) chains))))
;; this works, because the middle element is only contains one element

(def chains-5
     (filter #(not (nil? %)) (map #(fits-chain-back-and-front % all) all-3-chains)))

(comment;delete this
  (def all-5-chains (map #(concat (:front %) (:middle %) (:back %))
				(filter #(not (nil? %)) (map #(fits-chain-back-and-front % all) all-3-chains)))))

(deftest test-chains-5
  ;; the middle element contains 3 elements
  ;; back/front contains suggestions for the 4th and 5th elements
  (is  (every? #(= 3 %) (map #(count %) chains-5))))

;; (count all-5-chains)
;; 126

;; again, we have to utilize the same cartesian-product trick
(def chain-5-product (map  #(cartesian-product (:front %) (list (:middle %)) (:back %)) chains-5))
;;(take 2 chain-5-product)
;;((({:front 10, :back 33, :type :hept} ({:front 33, :back 21, :type :tria} {:front 21, :back 33, :type :octa} {:front 33, :back 64, :type :squa}) {:front 64, :back 41, :type :hexa}))
;; (({:front 10, :back 33, :type :hept} ({:front 33, :back 21, :type :tria} {:front 21, :back 33, :type :octa} {:front 33, :back 21, :type :hexa}) {:front 21, :back 48, :type :hept})
;;  ({:front 10, :back 33, :type :hept} ({:front 33, :back 21, :type :tria} {:front 21, :back 33, :type :octa} {:front 33, :back 21, :type :hexa}) {:front 21, :back 47, :type :pent})
;;  ({:front 10, :back 33, :type :hept} ({:front 33, :back 21, :type :tria} {:front 21, :back 33, :type :octa} {:front 33, :back 21, :type :hexa}) {:front 21, :back 16, :type :squa})))

;;(map  #(count %) (map #(map (fn [prod] (flatten prod)) %) (take 2 chain-5-product)))
;;((({:front 10, :back 33, :type :hept} {:front 33, :back 21, :type :tria} {:front 21, :back 33, :type :octa} {:front 33, :back 64, :type :squa} {:front 64, :back 41, :type :hexa}))
;; (({:front 10, :back 33, :type :hept} {:front 33, :back 21, :type :tria} {:front 21, :back 33, :type :octa} {:front 33, :back 21, :type :hexa} {:front 21, :back 48, :type :hept})
;;  ({:front 10, :back 33, :type :hept} {:front 33, :back 21, :type :tria} {:front 21, :back 33, :type :octa} {:front 33, :back 21, :type :hexa} {:front 21, :back 47, :type :pent})
;;  ({:front 10, :back 33, :type :hept} {:front 33, :back 21, :type :tria} {:front 21, :back 33, :type :octa} {:front 33, :back 21, :type :hexa} {:front 21, :back 16, :type :squa})))

;; (count (apply union (map #(map (fn [prod] (flatten prod)) %) (take 2 chain-5-product))))
;; 4

(deftest test-chain-5-product
  (is (= (map #(count %) (take 2 chain-5-product))
	 (map #(count %) (map #(map (fn [prod] (flatten prod)) %) (take 2 chain-5-product))))))



(def all-5-chains-unreduced
     (map  #(cartesian-product (:front %) (list (:middle %)) (:back %)) chains-5))

(deftest test-all-5-chains
  (is (= (reduce + (map #(* (count (:front %)) (list (count (:middle %))) (count (:back %))) chains-5))
	 747
	 (count all-5-chains-unreduced)
	 (count (apply union (map #(map (fn [prod] (flatten prod)) %) chain-5-product))))))

(def chains-5-union (apply union (map #(map (fn [prod] (flatten prod)) %) chain-5-product)))

(def all-5-chains (filter (fn [elem] (= 5 (count (into #{} (map #(:type %) elem))))) chains-5-union))


(defn missing-types [chain]
  (difference #{:tria :squa :pent :hexa :hept :octa} (into #{} (map #(:type %) chain))))

(defn missing-element [chain]
  (let [missing-type (first (missing-types chain))
	back (:front (first chain))
	front (:back (last chain))]
    {:front front :back back :type missing-type}))

;; now we just have to run through all the missing-elements for all-5-chains and see if one of them
;; are contained in the set of all the possible elements
(def all-elements (into #{} all))

(defn problem061 []
  (let [target  (first (filter #(contains? all-elements (missing-element %)) all-5-chains))
	element (missing-element target)
	complete (union target (list element))]
    (reduce +
            (map #(+ (* 100 (:front %)) (:back %)) complete))))
;; (5329 2926 2628 2821 2147 4753)
;; 20604 <<-- wrong .. produced with the wrong definition of heptagonals

;; with the correct definition of heptagonals, we get another answer
;; (8256 5625 2512 1281 8128 2882)
;; 28684

(deftest test-problem
     (is (= 8256 (last (take-while #(<= % 8256) squares))))
     (is (= 5625 (last (take-while #(<= % 5625) heptagonals))))
     (is (= 2512 (last (take-while #(<= % 2512) triangles))))
     (is (= 1281 (last (take-while #(<= % 1281) octagonals))))
     (is (= 8128 (last (take-while #(<= % 8128) pentagonals))))
     (is (= 2882 (last (take-while #(<= % 2882) hexagonals)))))




