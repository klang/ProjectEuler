(ns problem060
  (meta
   {:description "The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime." 
    :hint "the first two usable primes will be 3 and 7"
    })
  (:use tools.numbers)
  (:use tools.primes)
  (:use problem038)
  ;(:use [clojure.contrib.lazy-seqs :only (primes)])
  (:use clojure.contrib.combinatorics)
  (:use clojure.contrib.repl-utils)
  (:use clojure.set)
  (:use clojure.test))

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
	 limit 10                           ;;
	 ]
    (do (println {:first-elements (map #(list (first %) (first (second %))) queue)
		  :next-element   (next-element-index queue)
		  :first-dead     (first dead)
		  :next-prime     (first primes)
		  :candidate      candidate
		  :iteration      limit
		  }))
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
		     (dec limit))

	      ))
      )))

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
  (def candidate (conj #{} (first primes)))
  (def primes (rest primes))
  (defn first-elements [coll]
    (map #(list (first %) (first (second %))) coll))
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

	)
      )
    ))