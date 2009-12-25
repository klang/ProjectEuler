;; Add all the natural numbers below one thousand that are multiples of 3 or 5.

;;Find the sum of all the multiples of 3 or 5 below 1000.

(defn problem001 [n]
  (reduce + (map #(if (or (= 0 (mod % 5)) (= 0 (mod % 3))) % 0) (range n))))

;;If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
(deftest test-problem001
  (is (= 23 (problem001 10))))


