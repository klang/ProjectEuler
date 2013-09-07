(ns problem048
  (meta {:description "
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000."})
  (:use 
   [clojure.test :only (deftest is)]
   [clojure.math.numeric-tower :only (expt)]))

(defn solve048 [n] 
  (reduce + (map #(expt % %) (range 1 (+ 1 n)))))

(deftest test-problem048
  (is (= 10405071317 (solve048 10))))

;(problem048 1000)
;manually select the last 10 chars

;; user> (time (problem048 1000))
;;

;; (run-tests)

;; easier way to select the last ten digits..
(defn problem048 []
  (mod (solve048 1000) (expt 10 10)))
; 9110846700
