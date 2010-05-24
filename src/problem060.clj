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

;; let's just work with limited primes
(def limited-primes (primes-up-to 5000))

(comment
  (defn prime-concat? [[a b]]
    (and (prime? (concat-numbers a b)) 
	 (prime? (concat-numbers b a))))

  (every? #(prime-concat? %) (combinations [3 7 109 673] 2))
  (filter prime-concat? (map #(list 3 %) limited-primes))
  (filter prime-concat? (map #(list 7 %) limited-primes))
  )
(defn p-concat [p] (into #{} (filter #(prime-concat? p %) limited-primes)))

;; p7 #{673 229 2503 109 2707 1237 823 4729 541 4159}
;(let [p3 (p-concat 3), p7 (p-concat 7)] (map  #(intersection p3 p7 (p-concat %)) p7))

;(#{} #{} #{} #{109} #{673 229 2503 109 2707 1237 823 4729 541 4159} #{109} #{541} #{229 1237 823} #{} #{} #{1237} #{} #{} #{} #{} #{} #{} #{} #{823} #{229 4159} #{} #{541} #{1237} #{673} #{} #{} #{2707 1237} #{1237} #{} #{} #{} #{4159} #{} #{} #{} #{229} #{1237} #{} #{} #{4729} #{109} #{} #{} #{2707} #{109} #{109} #{} #{} #{109} #{} #{} #{} #{2503 1237 541} #{2707} #{1237} #{} #{} #{4159} #{} #{} #{} #{1237} #{541} #{} #{4159} #{} #{})

;(let [pa (p-concat 3), pb (p-concat 11)] (map  #(intersection pa pb (p-concat %)) pb))

(map #(list % (count (p-concat %))) (take 10 limited-primes))