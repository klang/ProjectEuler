(ns problem001
  (meta {:description "Add all the natural numbers below one thousand that are multiples of 3 or 5.

Find the sum of all the multiples of 3 or 5 below 1000.

If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
"})
  (:use clojure.test))

(defn- solve001 [n]
  (reduce + (map #(if (or (= 0 (mod % 5)) (= 0 (mod % 3))) % 0) (range n))))

(deftest test-problem001
  (is (= 23 (solve001 10))))

;; (time (solve001 1000))
;; "Elapsed time: 2.342757 msecs"
;; 233168

(defn problem001 [] (solve001 1000))
