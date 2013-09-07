(ns problem034
  (:use [clojure.test :only (deftest is)]
        [tools.numbers :only (digits factorial)]))

(deftest test-problem034
  (is (= 145 (reduce + (map #(factorial %) (digits 145))))))

(defn matches-sum-of-facforials? [n]
  (= n (reduce + (map #(factorial %) (digits n)))))

;; (filter #(matches-sum-of-facforials? %) (range 3 1000000))
;; (145 40585)
;; let us just try
;; user> (+ 145 40585)
;; 40730

(defn problem034 []
  (reduce + (filter #(matches-sum-of-facforials? %) (range 3 1000000))))

;; it works, but a limiting function should be possible to make
;; (expt 10 (count (digits 145)))

;; 7*9!=2540160<9999999 => all solutions are below 2540160 
