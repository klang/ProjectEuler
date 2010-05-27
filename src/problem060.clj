(ns problem060
  (meta
   {:description "The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime." 
    :hint "the first two usable primes will be 3 and 7"
    })
  (:use tools.numbers)
  (:use tools.primes)
  (:use problem038)
  (:use [clojure.contrib.lazy-seqs :only (primes)])
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

(comment
  (defn prime-concat? [[a b]]
    (and (prime? (concat-numbers a b)) 
	 (prime? (concat-numbers b a))))

  (every? #(prime-concat? %) (combinations [3 7 109 673] 2))
  (filter prime-concat? (map #(list 3 %) limited-primes))
  (filter prime-concat? (map #(list 7 %) limited-primes))
  )
(defn p-concat [p] (into #{} (filter #(prime-concat? p %) limited-primes)))
(defn p-concat-lazy [p] (filter #(prime-concat? p %) limited-primes))

; (map #(list % (count (p-concat %))) (take 10 limited-primes))

(defn p-check [& primes]
  "retuns the set of primes that can be concatenated with each prime in the primes given. The best effect is reached, if the primes given are members of each others p-maps. (p-check 3 7 109) will return the last member of the group, 673"
  (reduce intersection (map #(p-concat %) primes)))

(defn p-maps [prime-collection]
  "for each prime in the collection, returns a set {p #{ primes that can be concatenated with p}}"
  (loop [catch {}
	 primes prime-collection]
    (if (empty? primes)
      catch
      (recur (assoc catch (first primes)  (p-concat (first primes)))
	     (rest primes)))))

(def p-maps-sequence (map #(hash-map % (p-concat %)) limited-primes))

;; --------------------------------------------------------- depth first search

;; queue: {3 [7 11 17 ..]}
;; candidate: [3]
;; add candidate element to queue if (every? true? (map  #(contains? (p-concat 7) %) [3]))
;; ??? 
;;; PROFIT
;; queue: {3 [7 11 17 ..] 7 [3 19 61 ..]}
;; find candidate element in (last candidate), if all memebers of 
;; (every? true? (map  #(contains? (p-concat 109) %) [3 7]))

;; (drop-while #(<= % (reduce max [3 7])) (sort (p-concat 109)) )

(defn p-concat-lazy [p] (filter #(prime-concat? p %) limited-primes))

(defn add-element [coll prime]
  "an element is a prime and the lazily generated set of primes satisfying prime-concat?" 
  (letfn [(p-concat [n] (filter #(prime-concat? p %) limited-primes))]
    (assoc coll prime (p-concat prime))))

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

(def queue (sorted-map))
(def queue (add-element queue 3))
(def queue (add-element queue 7))
(def queue (add-element queue 11))
(def queue (add-element queue 19))
;;(def queue (sorted-map 3 (p-concat-lazy 3) 7 (p-concat-lazy 7) 11 (p-concat-lazy 11) 19 (p-concat-lazy 19)))
;;(def queue (hash-map 3 (p-concat-lazy 3) 11 (p-concat-lazy 11) 7 (p-concat-lazy 7)  19 (p-concat-lazy 19)))

(defn depth-first-search [length]
  (loop [queue (add-element (sorted-map) 3) ;; queue of elements to search
	 dead []                            ;; deadends, every prime up to (first dead) and all dead are deadends
	                                    ;; i.e. 109 can be a deadend, even if 11 hasn't been examined yet
	 primes (rest limited-primes)       ;; it should be possible to use lazy sequence primes instead
	 candidate [3]                       ;; 3,7,109,683 should be the first candidate of length 4
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
	    index (second next-element)]
	
	;; if index is in candidate, don't add the element to the queue, add the prime to the queue, drop the index and recur
	;; 
	(recur (if (nil? (queue index)) (add-element queue 7) queue) 
	       dead 
	       (if (= index (first primes)) (rest primes) primes)
	       (conj candidate index)
	       (dec limit)))
      )))


