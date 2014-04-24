(ns problem235
  (:use [clojure.math.numeric-tower :only (expt)]
	[clojure.test]))

(defn u [k r] (* (- 900N (* 3 k)) (expt r (- k 1N))))
(defn s [n r] (reduce + (map #(u % r) (range 1 (+ n 1N)))))

;; find r such that s(5000) = - 600000000000
;; Give your answer rounded to 12 places behind the decimal point.
