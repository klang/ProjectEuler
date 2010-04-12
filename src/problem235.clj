(ns problem235
  (:use [clojure.contrib.math :only (expt)]
	[clojure.test]))

(defn u [k r] (* (- 900 (* 3 k)) (expt r (- k 1))))
(defn s [n r] (reduce + (map #(u % r) (range 1 (+ n 1)))))

;; find r such that s(5000) = - 600000000000
;; Give your answer rounded to 12 places behind the decimal point.