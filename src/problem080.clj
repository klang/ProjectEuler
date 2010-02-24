(load "tools")
(load "problem064")
(load "problem065")

(use 'clojure.contrib.math)
(= 475 (reduce + (take 100 (digits (first (exact-integer-sqrt (* 2 (expt 10 200))))))))

(defn sum-digits [n]
  (reduce + (take 100 (digits (first (exact-integer-sqrt (* n (expt 10 200))))))))

(def irrational-squares
     (filter (fn [n] (not (rational? (sqrt n)))) (iterate inc 1)))

; (map #(sum-digits %) (take-while #(< % 100) irrational-squares))
(reduce + (map #(sum-digits %) (take-while #(< % 100) irrational-squares)))