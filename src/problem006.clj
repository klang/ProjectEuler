;; What is the difference between the sum of the squares and the square of the sums?
(ns problem006
    (:use clojure.contrib.math)
    (:use clojure.contrib.test-is))

(defn solve006 [n]
  (- (expt (reduce + (range (+ 1 n))) 2) 
     (reduce + (map #(expt % 2) (range (+ 1 n))))))

(deftest test-problem006
  (is (= 2640 (solve006 10))))

;; problem006> (time (problem006 100))
;; "Elapsed time: 3.953298 msecs"
;; 25164150

(defn problem006 [] (solve006 100))