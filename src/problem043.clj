(load "tools")
(use 'clojure.contrib.combinatorics)

(def pandigital (permutations '(0 1 2 3 4 5 6 7 8 9)))
;; user> (factorial 10)
;; 3628800

;; the easy way to convert back to a number
(defn integer [l] 
  ;; alternatively use BigInteger 
  (Integer. (apply str l)))

(partition 3 1 (rest '(1 2 3 4 5 6 7 8 9 0)))
(take 5 (map #(partition 3 1 (rest %)) pandigital))
(def test-number-t1 (partition 3 1 (rest (digits 1406357289))))
(def test-number-t2 (partition 3 1 (rest (digits 1460357289))))
(def test-number-f1 (partition 3 1 (rest (digits 1460537289))))

(defn divisible [num div]
  (= 0 (rem num div)))

(defn special-property [n]
  (map #(divisible (integer %1) %2) n primes)
  )

;;(reduce #(and %1 %2) (special-property test-number-f1))

;;
(reduce #(and %1 %2) (repeat false))
;; -- will never finish, eventhough the first term is false

(every? #(and true %) (repeat false))
(every? #(and true %) (special-property test-number-f1))

(defn special-property? [n]
  (every? #(and true %) (map #(divisible (integer %1) %2) n primes)))

(defn special-property? [n]
  (every? #(and true %) (map #(divisible (integer %1) %2) (partition 3 1 (rest n)) primes)))

(special-property? (digits 1406357289))

;; (reduce + (filter #(special-property? %) pandigital))
