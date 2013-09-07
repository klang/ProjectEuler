(ns problem060
  (meta
   {:description "The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime." 
    :hint "the first two usable primes will be 3 and 7"
    })
  (:use 
   [clojure.test :only (deftest is)]
   [tools.primes :only (prime? primes primes-up-to)]
   [problem038 :only (concat-numbers)]
   [clojure.set :only (intersection union)]
   [clojure.math.combinatorics :only (combinations)]))

(defn prime-concat? [a b]
  (and (prime? (concat-numbers a b)) 
       (prime? (concat-numbers b a))))

(deftest test-example
  (is (every? (fn [[a b]] (prime-concat? a b)) (combinations [3 7 109 673] 2))))

(defn working-set [coll]
  (every? (fn [[a b]] (prime-concat? a b)) (combinations coll 2)))


(deftest test-working-set
  (is (working-set [3]))
  (is (working-set [3 7]))
  (is (working-set [3 7 109]))
  (is (working-set [3 7 109 673])))

;; let's just work with limited primes and not 2 or 5
(def limited-primes (filter #(not (or (= 2 %) (= 5 %))) (primes-up-to 5000)))

(defn p-concat-set  [p] (into #{} (filter #(prime-concat? p %) limited-primes)))
(defn p-concat [p] (filter #(prime-concat? p %) limited-primes))

(defn p-check [& primes]
  "retuns the set of primes that can be concatenated with each prime in the primes given. The best effect is reached, if the primes given are members of each others p-maps. (p-check 3 7 109) will return the last member of the group, 673"
  (reduce intersection (map #(p-concat-set  %) primes)))

(defn p-check-union [& primes]
  (reduce union (map #(p-concat-set  %) primes)))

(defn p-maps [prime-collection]
  "for each prime in the collection, returns a set {p #{ primes that can be concatenated with p}}"
  (loop [catch {}
	 primes prime-collection]
    (if (empty? primes)
      catch
      (recur (assoc catch (first primes)  (p-concat-set  (first primes)))
	     (rest primes)))))

(def p-maps-sequence (map #(hash-map % (p-concat-set  %)) limited-primes))

;; --------------------------------------------------------- depth first search

(defn add-element [coll prime]
  "an element is a prime and the lazily generated set of primes satisfying prime-concat?" 
  (letfn [(p-concat-set  [n] (filter #(prime-concat? n %) limited-primes))]
    (assoc coll prime (p-concat-set  prime))))


(defn add-elements [coll & prime-numbers]
  "an element is a prime and the lazily generated set of primes satisfying prime-concat?" 
  (letfn [(p-concat-set  [n] (filter #(prime-concat? n %) limited-primes))]
    (reduce into coll (map #(sorted-map % (p-concat-set  %)) prime-numbers))))

(defn pop-element-from-prime [coll prime]
  (assoc coll prime (rest (coll prime))))

(defn second-element-min
  "given a collection of pairs, returns the pair that has the smallest second element. "
  ; should deliver (7, 3) over (11, 3)
  ([x] x)
  ([x y] (if (and (not (or (nil? (second x)) (nil? (second y))))
	      (<= (second x) (second y))) x y))
  ([x y & more]
   (reduce second-element-min (second-element-min x y) more)))

(defn first-elements [coll]
  (map #(list (first %) (first (second %))) coll))

(defn next-element-index [coll]
  "return the smallest element, that has the smallest first element. 7->3,.. 11->3,... returns (7,3)"
  (reduce second-element-min (first-elements coll)))

;;
;; the first tow steps that contains 3 and 7 .. 
;;problem060> (filter #(= '(3 7) (take 2 (sort (p-concat-set  %)))) (sort (p-concat-set  7)))
;;(109 229 541 673 823 1237 2503 2707 4159 4729)
;;
(defn next-step [& steps]
  (filter #(= steps (take 2 (sort (p-concat-set  %)))) (sort (p-concat-set  (last steps)))))
;; .. just a slow way to do a p-check .. 
;;problem060> (filter #(= '(3 7) (take 2 (sort (p-concat-set  %)))) (sort (p-concat-set  7)))
;;(109 229 541 673 823 1237 2503 2707 4159 4729)
;;problem060> (sort (p-check 3 7))
;;(109 229 541 673 823 1237 2503 2707 4159 4729)
(comment
;; initializing
  (def queue (sorted-map))
  (def candidate #{})
  (def primes limited-primes)

;; main loop
  (def queue (add-element queue (first primes)))
;; queue: {3 [7 11 17 ..]}
  (def candidate (conj #{} (first primes)))
;; candidate: [3]
;; seaching for sets starting with 3
  (def primes (rest primes))
;; primes: [7 11 13 ... ]
  ;; -- add elements to queue, (from primes) until next-element shows an element already in candidate
  (def next-element (next-element-index queue))
  (defn add-more-elements-to-queue [queue candidate primes]
    (loop [queue (add-element queue (first primes)) candidate candidate primes (rest primes)]
      (if (contains? (second (next-element-index queue))))
      )
  )
  (defn next-prime [prime candidate] 
    "starting with a specific candidate, what is the next element?"
    (first (drop-while #(contains? candidate %) (p-concat prime))))

  (def queue (add-element queue (next-prime (first next-element) candidate)))
  (def queue (pop-element-from-prime queue 7))
  (def next-element (next-element-index queue))
  ;; next element has to select the next element that is not already in the candidate

;; next-element: (3 7)
;; next element is not in queue .. insert it
  
  (queue (second next-element))
  (def queue (add-element queue (second next-element)))
;; (first-elements queue)
  (= (first primes) (second next-element)) --- maybe primes should be culled at some point?
  (def next-element (next-element-index queue))
  ;; (if (< (second next-element) (first next-element) ) )
  ;; pop the first element from (first next-element) 
  (def queue (pop-element-from-prime queue (first next-element)))
  ;; next element index does not return the correct index, when we start taking out elements
  ;; we have to remember what we have in the candidate [3 7] and find the next element among 
  ;; the elements containing those values. i.e. the first element in 7:[19 61 ..]
  (next-element-index queue)

(first-elements queue)
  (def queue (pop-element-from-prime queue 3))

;; add candidate element to queue 
;; (every? true? (map  #(contains? (p-concat-set  7) %) [3])) <== implicit from the fact that 7 is a memeber of (p-concat-set  3)

;; queue: {3 [7 11 17 ..] 7 [3 19 61 ..]}
;; find candidate element in (last candidate), if all memebers of 
;; (every? true? (map  #(contains? (p-concat-set  109) %) [3 7]))

;; (drop-while #(<= % (reduce max [3 7])) (sort (p-concat-set  109)) )


  (def queue (add-element queue 7))
  (def queue (add-element queue 11))
  (def queue (add-element queue 19)))
;;(def queue (sorted-map 3 (p-concat 3) 7 (p-concat 7) 11 (p-concat 11) 19 (p-concat 19)))
;;(def queue (hash-map 3 (p-concat 3) 11 (p-concat 11) 7 (p-concat 7)  19 (p-concat 19)))

(defn depth-first-search [length]
  (loop [queue (add-element (sorted-map) 3) ;; queue of elements to search
	 dead []                            ;; deadends, every prime up to (first dead) and all dead are deadends
	                                    ;; i.e. 109 can be a deadend, even if 11 hasn't been examined yet
	 primes (rest limited-primes)       ;; it should be possible to use lazy sequence primes instead
	 candidate #{3}                     ;; 3,7,109,683 should be the first candidate of length 4
	 limit 10]
    (do (println {:first-elements (map #(list (first %) (first (second %))) queue)
		  :next-element   (next-element-index queue)
		  :first-dead     (first dead)
		  :next-prime     (first primes)
		  :candidate      candidate
		  :iteration      limit}))
    (if (or (= length (count candidate)) (zero? limit))
      queue
      (let [next-element (next-element-index queue)
	    element (first next-element)
	    index (second next-element)]
	;; if index is in candidate, don't add the element to the queue, add the prime to the queue, drop the index and recur
	(cond (contains? candidate index)
	      (list next-element element index)
	      (not (contains? candidate index))
	      (recur (if (nil? (queue index)) (add-element queue index) queue) 
		     dead 
		     (if (= index (first primes)) (rest primes) primes)
		     (conj candidate index)
		     (dec limit)))))))

(comment
(recur (pop-element-from-prime queue prime)
		    dead
		    primes
		    candidate
		    (dec limit))
)

;; -------------- intersection approach (time critical)

;; [3]: #{7 11 17 31 37 59 67..
;; 7:
;; [3 7]: #{109 229 541 673 823 1237 2503 2707 4159 4729}
;; 109:
;; [3 7 109] #{673}
;; 229: (first (sort (difference (intersection (p-check 3 7)) #{109 673})))
;; [3 7 229] #{}
;; 541: (first (sort (difference (intersection (p-check 3 7)) #{109 673 229})))
;; [3 7 541] #{4159}
;; 823: (first (sort (difference (intersection (p-check 3 7)) #{109 673 229 541 4159})))
;; [3 7 823] #{}
;; 1237: (first (sort (difference (intersection (p-check 3 7)) #{109 673 229 541 4159 823})))
;; [3 7 1237] #{}
;; 2503: (first (sort (difference (intersection (p-check 3 7)) #{109 673 229 541 4159 823 1237})))
;; [3 7 2503] #{}
;; 2707: (first (sort (difference (intersection (p-check 3 7)) #{109 673 229 541 4159 823 1237 2503})))
;; [3 7 2707] #{}
;; 4729: (first (sort (difference (intersection (p-check 3 7)) #{109 673 229 541 4159 823 1237 2503 2707})))
;; [3 7 4729] #{}
;; nil: (first (sort (difference (intersection (p-check 3 7)) #{109 673 229 541 4159 823 1237 2503 2707 4729})))


;; [3 11]: (sort (p-check 3 11)) : #{701 2069 2213 2297 2843}
;; 701: (first (sort (difference (intersection (p-check 3 11)) #{})))
;; (sort (p-check 3 11 701)) #{}
;; 2069: (first (sort (difference (intersection (p-check 3 11)) #{701})))
;; (sort (p-check 3 11 2069)) #{2297}
;; 2213: (first (sort (difference (intersection (p-check 3 11)) #{701 2069 2297})))
;; (sort (p-check 3 11 2213)) #{}
;; 2843: (first (sort (difference (intersection (p-check 3 11)) #{701 2069 2297 2213})))
;; (sort (p-check 3 11 2843)) #{}
;; nil: (first (sort (difference (intersection (p-check 3 11)) #{701 2069 2297 2213 2843})))

;;;

(defn smaller-than [prime]
  (take-while #(< % prime) (sort (p-concat-set prime))))

(defn potential-length [prime]
  (count (smaller-than prime)))

;; (smaller-than 673)
;; (3 7 109 199 397 457 499 613)

;; --- one candidate starting in each seen prime, approach.
;; --- 
(comment
  (def queue (assoc queue (/ (reduce + #{3 11}) (count #{3 11}))
		    (struct candidate #{3 11} (reduce + #{3 11}) (p-check 3 11))
		    )))

;;-- intersection approach, modified with lazy sequences

(comment
  (filter #(= '(3 7) (take 2 (sort (p-concat-set  %)))) (sort (p-concat-set  7)))

  (def primes limited-primes)
  (def queue (sorted-map))
  (def candidate #{})
  (def queue (add-element queue (first primes)))
  (def candidate (conj candidate (first primes)))
  (def primes (rest primes))
  (smallest-sum-index queue)
  (def queue (add-element queue (first (queue 3))))
  (def queue (pop-element-from-prime queue (first (smallest-sum-index queue))))
  (def candidate (conj candidate (first (smallest-sum-index queue))))

  (smallest-sum-index queue)
  (def queue (add-element queue (first (queue (first (smallest-sum-index queue))))))
  (def queue (pop-element-from-prime queue (first (smallest-sum-index queue))))
  ;;11 is in the queue .. but starts with 3 .. 3->11 is a fair possibility

  (smallest-sum-index queue)

  (defn first-elements [coll]
    (map #(list (first %) (f irst (second %))) coll))

  (defn next-element-index [coll]
    "return the smallest element, that has the smallest first element. 7->3,.. 11->3,... returns (7,3)"
    (reduce second-element-min (first-elements coll)))

  (defn smallest-sum-index [coll]
    (reduce second-element-min (map (fn [[a b]] (list a (+ a b))) (first-elements queue))))

  (defn find-candidate [coll candidate]
    ((map #()))
    )
  (first-elements queue))

(defn p-search [length]
  (loop [look-at 0
	 queue (sorted-map)
	 primes limited-primes
	 candidate #{}
	 limit 200]
    (do (println {:look-at        (last (sort candidate))
		  ;;		  :first-elements (map #(list (first %) (first (second %))) queue)
		  :foo (take-while #(<= % look-at) (queue (first (queue look-at))))
		  :next (first primes)
		  :last (first (last queue))
		  :candidate      candidate
		  :iteration      limit
		  
		  }))
    (if (or (= length (count candidate)) (zero? limit))
      candidate
      (cond (empty? queue)
	    (recur (first primes) 
		   (add-element queue (first primes)) 
		   (rest primes) 
		   (conj candidate (first primes))
		   (dec limit))
	    ;; is the there an element in the queue
	    ;; that can be added to the candidate?
	    ;; no, then add the next prime to the queue
	    (not (contains? queue (first (queue look-at))))
	    (recur look-at
		   (add-element queue (first primes))
		   (rest primes)
		   candidate
		   (dec limit))
	    ;; yes, add that element to the candidate
	    ;; and start looking at THAT element
;;	    (and) (contains? queue (first (queue look-at)))
	    (= candidate 
	       (into #{} (take-while #(<= % look-at) (second (last queue)))))
		 
	    (do
	      ;; we should only change look-at element if
	      ;; all elements from the candidate are found
	      (println candidate 
		       (first (last queue))
		       (take-while #(<= % look-at) (second (last queue)))
		       (= candidate 
			  (into #{} (take-while #(<= % look-at) (second (last queue)))))
		       )
	      (recur (first (last queue))
		     (pop-element-from-prime queue look-at)
		     primes
		     (conj candidate (first (last queue)))
		     (dec limit)))
	    (contains? queue (first (queue look-at)))
	    (do ;(println "adding prime to queue because of first element")
	      (recur look-at
		     (add-element queue (first primes))
		     (rest primes)
		     candidate
		     (dec limit)))
	    :else
	    nil
	    ;; contains? has to check every element in the candidate?
	    ;; at some point
	))))

;; how about just using a lazy sequence?

(defn p2 []
  "take a limited-prime .. then check if there are any values below the selected number that produce a working set"
  (for [a limited-primes
	b (drop-while #(< % a) (p-concat a))
	:when (working-set [a b])]
    [a b]))
;; --- and only take the first element ..

;;(first (p2))
;;[3 7]

(defn p3 []
  (for [a limited-primes
	b (drop-while #(< % a) (p-concat a))
	c (drop-while #(< % b) (p-concat b))
	:when (working-set [a b c ])]
    [a b c]))

;;(first (p3))
;;[3 7 109]

(defn p4 []
  (for [;a (drop 1 limited-primes)
	a  limited-primes
	b (drop-while #(< % a) (p-concat a))
	c (drop-while #(< % b) (p-concat b))
	d (drop-while #(< % c) (p-concat c))
	:when (working-set [a b c d])]
    [a b c d]))

;;problem060> (time (first (p4)))
;;"Elapsed time: 933.828194 msecs"
;;[3 7 109 673]

;; technically, this does work, but it takes way more than a minute.

;; to make sure, that we are not just cycling through all the cominations 
;; in limited primes, we use a version of prime-concat that uses the full
;; sequence of primes

(defn p-concat-f [p] (filter #(prime-concat? p %) primes))

(defn p5 []
  (for [a limited-primes
	b (drop-while #(< % a) (p-concat-f a))
	c (drop-while #(< % b) (p-concat-f b))
	d (drop-while #(< % c) (p-concat-f c))
	e (drop-while #(< % d) (p-concat-f d))
	:when (working-set [a b c d e])]
    [a b c d e]))

;; ------------------------ another stab at this problem ----------------------

(defn producer [prime-concat-vector]
  "returns a sequence of primes starting after the largest element of a vector"
  (drop-while #(<= % (reduce max prime-concat-vector)) primes))

(defn next-prime [prime]
  "returns a sequence of primes starting after the largest element of a vector"
  (first (drop-while #(<= % prime) (lazy-cat [3] (drop 3 primes)))))

(def queue #{{:length 3 :group [3] :count 1 :checked 3 :weight 1}})
;; take out element of the queue: {:length 3 :group [3] :checked 3}
;; find the next prime, starting from :checked (next-prime 3)
;; if members of :group and the next prime form a working-set (working-set (conj [3] 5))
;; --> then a new element is inserted in the queue
;; either way, an element with {:length prime :group [prime] :checked prime} is inserted
(def queue #{{:length 3 :group [3] :count 1 :checked 5 :weight 2} 
	     {:length 5 :group [5] :count 1 :checked 5 :weight 1}})
;; take out the element with the lowest length: {:length 3 :group [3] :checked 7}
;; (how do we avoid 5 after a while?)
;; find the next prime, starting from :checked (next-prime 5)
;; (working-set (conj [3] 7)) is true {:length 10 :group [3 7] :checked 7} is inserted
;; {:length 7 :group [7] :checked 7} is inserted
(def queue #{{:length 3 :group [3] :count 1 :checked 7 :weight 3}
	     {:length 5 :group [5] :count 1 :checked 5 :weight 1}
	     {:length 10 :group [3 7] :count 2 :checked 7 :weight 1}
	     {:length 7 :group [7] :count 1 :checked 7 :weight 1}})

;; simply add a weight, telling how many times the element has been processed

(defstruct element :length :group :count :checked :weight :selection)
(def queue #{(struct-map element :length 3 :group [3] :count 1 :checked 3 :weight 1)})
(def queue #{(struct-map element :length 3 :group [3] :count 1 :checked 5 :weight 2)
	     (struct-map element :length 5 :group [5] :count 1 :checked 5 :weight 1)})
(defn f [length]
     ;; seed the loop with the first logical element
     ;; we might have to just jump 2 and 5 and be over with it
     ;; (lazy-cat [3] (take 10 (drop 3 primes)))
  (loop [queue #{(struct-map element :length 3 :group [3] :count 1 :checked 3 :weight 1 :selection 6)}
	    limit 50]
       ;; if there is an element with the right length, return it and terminate
    ;; (filter #(not (nil? %))) contains nil
        (if (or (zero? limit) (= length (:count (first (sort-by :count > queue)))))
	 (list limit (count queue) 
	       (:checked (first (sort-by :checked > queue)))
	       (first (sort-by :count > queue))
	       (filter #(<= 4 (:count %)) queue))
	 ;queue
	 (let [;e (first (sort-by :count > queue))
	       e (first (sort-by :selection < queue))
	       ;e (first (sort-by :checked < queue))
	       ;e (first (sort-by :checked < queue))
	       p (next-prime (:checked e))
	       l (+ (:length e) p)
	       g (conj (:group e) p)
	       q (conj (disj queue e) 
			(struct-map element :length (:length e) :group (:group e) 
				    :count (:count e)
				    :checked p
				    :weight (inc (:weight e))
				    :selection (quot (+ l (:weight e)) (:count e))
				    ;:selection (+ (inc (:weight e)) (quot l  (:count e)))
				    )
			(struct-map element :length p :group [p] :count 1 :checked p :weight 1 
				    :selection (+ p p)))] 
	   ;; q now contains the new elements that have to go into the queue in the next recursion
	   ;; but, if g is a working-set, an extra element have to be part of queue as well
	   (println (list limit (count queue) e))
	   (if (working-set g)
	     (recur (conj q (struct-map element :length l :group g 
				      :count (inc (:count e)) :checked p :weight 1
				      :selection l))
		    (dec limit))
	     (recur q (dec limit)))))))

(defn next-prime [prime]
  (first (drop-while #(< % prime) (p-concat-f prime))))

(defn p-concat-set  [p] (into #{} (filter #(prime-concat? p %) limited-primes)))
(defn p-concat [p] (filter #(prime-concat? p %) limited-primes))

(defn p-check [& primes]
  "retuns the set of primes that can be concatenated with each prime in the primes given. The best effect is reached, if the primes given are members of each others p-maps. (p-check 3 7 109) will return the last member of the group, 673"
  (reduce intersection (map #(p-concat-set  %) primes)))

(comment
  (defn p-concat-f [p] (filter #(prime-concat? p %) (drop-while #(<= % p) primes)))
)

;; ------------------------------------ stab number.., I fucking lost count..

(defstruct element :length :group)
(def queue [(struct-map element :length 3 :group [3]) 
	    (struct-map element :length 10 :group [3 7])
	    (struct-map element :length 7 :group [7])])
;; .. :length is not really needed

(defn prime-concat? [a b]
  (and (prime? (concat-numbers a b)) 
       (prime? (concat-numbers b a))))

(defn working-set [coll]
  (every? (fn [[a b]] (prime-concat? a b)) (combinations coll 2)))

(def prime-concat? (memoize prime-concat?))

;;(set! *warn-on-reflection* true)
;;an efficient version of working-set has to be made .. 

(defstruct element :count :group)

(defn new-elements [queue p]
  (for [q queue :when (working-set (conj (:group q) p))] 
    (struct-map element :count (inc (:count q)) :group (conj (:group q) p))))

(defn p60 [length]
  (loop [p (drop 3 primes)                                 ;; let's ignore 2 and 5 right away and initialize queue with 3 
	 queue [(struct-map element :count 1 :group [3])]  ;; a simple vector to keep the elements seen
	 m 1                                               ;; keep track of the longest group	
;	 limit 130                                         ;; limit the number of recursions (testing)  
	 ]
    (if #_(or (zero? limit)) (= m length) 
      (list #_limit (count queue) (first (sort-by :count > queue))) ;; just return the longest element.
      (let [extras (new-elements queue (first p))
	    this-element (struct-map element :count 1 :group [(first p)])]
	#_(do (println (list limit m (first p) this-element extras)))
	(if (= '() extras)
	  (recur (rest p) (conj queue this-element) m #_(dec limit))
	  (recur (rest p) 
		 (into (conj queue this-element) extras)
		 (max m (:count (first (sort-by :count > extras))))
		 #_(dec limit)))))))

;;problem060> (time (p60 5))
;;"Elapsed time: 796469.559427 msecs"
;;(21741 {:count 5, :group [13 5197 5701 6733 8389]})
;;problem060> (reduce + [13 5197 5701 6733 8389])
;;26033
(defn problem060 [] (reduce + (:group (second (p60 5)))))
