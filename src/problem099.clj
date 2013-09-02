(ns problem099
  (meta {:description "Comparing two numbers written in index form like 2^11 and 3^7 is not difficult, as any calculator would confirm that 2^11 = 2048  3^7 = 2187.

However, confirming that 632382^518061 > 519432^525806 would be much more difficult, as both numbers contain over three million digits."})
  (:use [clojure.string :only (split)]
        [clojure.set :only (union)]
	[clojure.test :only (deftest is)]))

;; using Math/log instead of 
;; (:use [clojure.contrib.generic.math-functions :only (log)])


;;(> (expt 632382 518061) (expt 519432 525806))
;; each number contains more than 3 million digits
;; don't even think about evaluating the term.

(def base-exp (slurp "src/base_exp.txt"))

(defn lt [[base1 exp1] [base2 exp2]]
  (< (* exp1 (Math/log base1)) (* exp2 (Math/log base2))))

(deftest test-lt
  (is (lt [519432 525806] [632382 518061]))
  (is (lt [2 11] [3 7])))

(defn string2integer [be]
  (let [base  (. Integer parseInt (nth be 0) 10)
	exp   (. Integer parseInt (nth be 1) 10)]
    (list base exp)))

;;(last (sort lt (map #(string2integer %) (map #(split % #",") (split base-exp #"\r\n")))))
;;(895447 504922) -> 709

(defn problem099 [] (last (sort lt (map #(string2integer %) (map #(split % #",") (split base-exp #"\r\n"))))))
