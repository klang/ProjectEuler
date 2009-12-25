;; What is the difference between the sum of the squares and the square of the sums?
(use 'clojure.contrib.math
     'clojure.contrib.test-is)

(defn problem006 [n]
  (- (expt (reduce + (range (+ 1 n))) 2) 
     (reduce + (map #(expt % 2) (range (+ 1 n))))))

(deftest test-problem006
  (is (= 2640 (problem006 10))))