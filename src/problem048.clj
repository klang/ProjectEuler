; The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

;Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

(use 'clojure.contrib.test-is
     '[clojure.contrib.math :only (expt)] 
     )

(defn problem048 [n] 
  (reduce + (map #(expt % %) (range 1 (+ 1 n)))))

(deftest test-problem048
  (is (= 10405071317 (problem048 10))))

(problem048 1000)
;manually select the last 10 chars

;; user> (time (problem048 1000))
;;

;; (run-tests)
