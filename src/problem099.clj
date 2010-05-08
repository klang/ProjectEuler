(ns problem099
  (:use tools.numbers)
  (:use tools.primes)
  (:use [clojure.contrib.str-utils2 :only (split)])
  (:use clojure.contrib.duck-streams)
  (:use clojure.test))

;;(> (expt 632382 518061) (expt 519432 525806))
;; each number contains more than 3 million digits
;; don't even think about evaluating the term.

;; maybe the factors can be used for something?
;; user> (factors 632382)
;; [2 3 105397]
;; user> (factors 518061)
;; [3 172687]
;; 
;; user> (factors 519432)
;; [2 2 2 3 23 941]
;; user> (factors 525806)
;; [2 19 101 137]

(def base-exp (slurp "base_exp.txt"))
(map #(list
       (factors (. Integer parseInt (nth % 0) 10)) 
       (factors (. Integer parseInt (nth % 1) 10))) 
     (take 5 (map #(split % #",") (split base-exp #"\r\n"))))

(defn factor-base-exp [be]
  (let [base  (. Integer parseInt (nth be 0) 10)
	exp   (. Integer parseInt (nth be 1) 10)
	basef (factors base)
	expf  (factors exp)]
    (list (list base exp) basef expf)))

(def factors-base-exp (map #(factor-base-exp %) 
			   (map #(split % #",") (split base-exp #"\r\n"))))
