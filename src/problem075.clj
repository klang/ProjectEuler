(ns problem075
  (meta {:description "It turns out that 12 cm is the smallest length of wire that can be bent to form an integer sided right angle triangle in exactly one way, but there are many more examples.

12 cm: (3,4,5)
24 cm: (6,8,10)
30 cm: (5,12,13)
36 cm: (9,12,15)
40 cm: (8,15,17)
48 cm: (12,16,20)

In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle, and other lengths allow more than one solution to be found; for example, using 120 cm it is possible to form exactly three different integer sided right angle triangles.

120 cm: (30,40,50), (20,48,52), (24,45,51)

Given that L is the length of the wire, for how many values of L <= 1,500,000 can exactly one integer sided right angle triangle be formed?

Note: This problem has been changed recently, please check that you are using the right parameters.
"})
  (:use clojure.contrib.math)
  (:use clojure.test))

(set! *warn-on-reflection* true)

(def known [ 12  24  30  36  40  48  56  70  72  80  96 108 
	    112 126 140 150 154 156 160 176 182 192 198 200 
	    204 208 216 220 224 228 234 260 276 286 306 308 
	    320 324 340 348 350 352 364 372 374 378 380 384 
	    392 400 416 418 442 444])

; http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Pythag/pythag.html#primPTgraphs

;; let's start by finding primitive pythagorean triangles.
;; having them, we can mark off the multipas of each in an int-array
;; and simply count the indices that has 1 as the value.

;; 3,4,5 (12) is the smallest primitive triangle, there are (/ 1500000 12) triangles
;; that also fulfills the condition, but some might belong to other primitive groups
;; as well and will be discounted.


(defn uad-tree [[a b h]]
  (let [aa (* 2 a) bb (* 2 b) hh (* 2 h) hhh (+ hh h)]
    #_[aa (+ a a) bb (+ b b) hh (+ h h) hhh (+ hh h)]
    {:up    (vector (+ (- a bb) hh) (+ (- aa b) hh) (+ (- aa bb) hhh))
     :along (vector (+ (+ a bb) hh) (+ (+ aa b) hh) (+ (+ aa bb) hhh))
     :down  (vector (+ (- bb a) hh) (+ (- b aa) hh) (+ (- bb aa) hhh))}))

(deftest test-uad-tree
  (is (= (uad-tree [3 4 5])
	 {:up [5 12 13], :along [21 20 29], :down [15 8 17]}))
  (is (= (uad-tree [5 12 13])
	 {:up [7 24 25], :along [55 48 73], :down [45 28 53]}))
  (is (= (uad-tree [21 20 29])
	 {:up [39 80 89], :along [119 120 169], :down [77 36 85]}))
  (is (= (uad-tree [15 8 17])
	 {:up [33 56 65], :along [65 72 97], :down [35 12 37]}))
  (is (= [5 12 13]  (:up (uad-tree [3 4 5]))))
  (is (= (uad-tree (:up (uad-tree [21 20 29])))
	 {:up [57 176 185], :along [377 336 505], :down [299 180 349]})))

(defn uad-tree-mn  [[m n]]
  {:up    (vector (- (* 2 m) n) m)
   :along (vector (+ (* 2 m) n) m)
   :down  (vector (+ m (* 2 n)) n)})

(deftest test-uad-tree-mn
  (is (= (uad-tree-mn [2 1])
	 {:up [3 2], :along [5 2], :down [4 1]}))
  (is (= (uad-tree-mn [3 2])
	 {:up [4 3], :along [8 3], :down [7 2]}))
  (is (= (uad-tree-mn [5 2])
	 {:up [8 5], :along [12 5], :down [9 2]}))
  (is (= (uad-tree-mn [4 1])
	 {:up [7 4], :along [9 4], :down [6 1]}))
  (is (= (uad-tree-mn (:up (uad-tree-mn [4 1]))))))

(defn mn2abh [[m n]]
  (vector (- (* m m) (* n n)) (* 2 m n) (+ (* m m) (* n n))))

(defn mn2abh [[m n]]
  (let [mm (* m m) nn (* n n) a (- mm nn)]
    (vector a (+ a 1) (+ mm nn)))) 
;;  (= (+ a 1) (* 2 m n))

;; int-array starts at 0
;;(def pyth (int-array 1500001 0)) 
;;(time (reduce + (filter #(= 1 %) pyth)))
;;"Elapsed time: 1294.859363 msecs"
;;0

;; problem075> (time (def v (vec (take 1500001 (cycle [0])))))
;; "Elapsed time: 7570.119881 msecs"
;; #'problem075/v
;; problem075> (time (def v (into [] (take 1500001 (cycle [0])))))
;; "Elapsed time: 3549.343479 msecs"
;; #'problem075/v

(defn inc-item [coll limit item]
  (loop [coll coll mark item]
    (if (<= mark limit)
      (recur (assoc coll mark (inc (get coll mark))) (+ mark item))
      coll)))

(defn inc-items [coll limit & items]
  (loop [coll coll items items mark (inc limit)]
    (if (empty? items)
      coll
      (if (<= mark limit)
	(recur (assoc coll mark (inc (get coll mark))) 
	       items 
	       (+ mark (first items)))
	(do (println (first items)) (recur coll
		   (rest items)
		   (first items)))))))

;; problem075> (time (def v2 (inc-item v (count v) 12)))
;; "Elapsed time: 553.110698 msecs"
;; #'problem075/v2
;; problem075> (time (reduce + (filter #(= 1 %) v2)))
;; "Elapsed time: 484.535483 msecs"
;; 125000

;; it takes a bit longer to allocate the vector, but each update is much faster .. 
;; each update can be done in parallel, with agents 
;; .. that might be a good exercise

(defn work []
  (let [uad (uad-tree [3 4 5])]
    (inc-item )
    )
  )



;Pell numbers: a(0) = 0, a(1) = 1; for n > 1, a(n) = 2*a(n-1) + a(n-2). 
(defn pell-numbers []
  "http://www.research.att.com/~njas/sequences/A000129"
  (map first (iterate (fn [[a b]] [(+ (* 2 a) b) a]) [0 1])))

(deftest test-pell-numbers
  (is (= (into [] (take 30 (pell-numbers))) 
	 [0 1 2 5 12 29 70 169 408 985 2378 5741 13860 33461 
	  80782 195025 470832 1136689 2744210 6625109 15994428 
	  38613965 93222358 225058681 543339720  1311738121 
	  3166815962 7645370045 18457556052 44560482149])))


;;;;------------------------ where the hell did I find the calculation below?
;;;;-------------------------It's wrong.
;(for [a (range 2, (+ (/ p 4) 1)) :when (zero? (mod (- (* p p) (* 2 p a)) (- (* 2 p) (* 2 a))))] 1) 

(def p 36)

(defn p75 [limit]
  (loop [c 0 p 1]
    (if (< limit p) c
      (let [t (reduce + (for [a (range 2 (+ (/ p 4) 1) )] 
			  (if (zero? (mod (- (* p p) (* 2 p a)) (- (* 2 p) (* 2 a)))) 1 0)))]
	(if (= 1 t)
	  (recur (inc c) (inc p))
	  (recur c (inc p)))))))


;; problem075> (time (p75 100))
;; "Elapsed time: 12.619484 msecs"
;; 10
;; problem075> (time (p75 1000))
;; "Elapsed time: 515.504529 msecs"
;; 104
;; problem075> (time (p75 10000))
;; "Elapsed time: 45606.770662 msecs"
;; 1069

(defn side [a b]
  (sqrt (+ (* a a) (* b b))))

(defn group [a b c]
  (map #(+ (* a %) (* b %) (* c %)) (iterate inc 1)))

(defn group [a b c]
  (map #(* (+ a b c) %) (iterate inc 1))) 
(defn g [n] (map #(* n %) (iterate inc 1)))

;; problem075> (time (mark-off-group (mark-off-group (int-array 1500000 0) 12 ) 30))
;; "Elapsed time: 17959.651634 msecs"
;; #<int[] [I@1feb3a6>


(defn checkp [p]
  (loop [a (range 2 (+ (/ p 4) 1))
	 t 0]
    (cond (< 1 t) false
	  (empty? a) (not (zero? t))
	  :else
	(if (zero? (mod (- (* p p) 
			   (* 2 p (first a))) 
			(- (* 2 p) (* 2 (first a)))))
	  (recur (rest a) (inc t)) 
	  (recur (rest a) t)))))

(deftest test-all
  (is (every? true? (map #(checkp %) known))))


;; (map #(hash-map :i %2 :v %1) (x 60) (iterate inc 0))
;; (filter #(not (nil? %)) (map #(if (= 1 %1) %2 nil) (x 121) (iterate inc 0)))

(defn x [limit]
  (let [limit (int limit) 
	coll (int-array (inc limit) 0)]
    (loop [i (int 1)]
      (if (<= limit i)
	coll
	(do 
	  (if (zero? (aget coll i))
	    (if (checkp i)
	      (do (println i) (mark-off-group coll i))))
	  (recur (inc i)))))))


(defn mark-off-group [coll grp]
  "add one to each multipa of a given integer"
  (let [limit (int (count coll)) i (int grp)]
    (loop [g i]
      (if (< g limit) 
	(do (aset coll g (inc (aget coll g)))
	    (recur (+ g i)))
	coll))))



