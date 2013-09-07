(ns problem062
  (:use 
   [clojure.test :only (deftest is)]
   [tools.numbers :only (digits)]
   [clojure.math.numeric-tower :only (expt) :as math]))

;The cube, 41063625 (345³), can be permuted to produce two other cubes: 56623104 (384³) and 66430125 (405³). In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.

;Find the smallest cube for which exactly five permutations of its digits are cube.

(defn foo [n]
  (loop [current (iterate inc 1) catch {}]
    (let [item (sort (digits (expt (first current) 3)))]
      (cond 
	;; if there are already (n-1) items in catch for this item, we are done
	(= (count (catch item)) (- n 1)) (conj (catch item) (first current))  
	:else
	(recur 
	 (rest current) 
	 (assoc catch item (conj (catch item) (first current))))))))

(deftest test-foo
  (= '(405 384 345) (foo 3)))

;; (time (foo 3))
;; "Elapsed time: 8.923483 msecs"
;; (405 384 345)

;; (time (foo 5))
;; "Elapsed time: 536.815609 msecs"
;; (8384 8288 7202 7061 5027)
;; (expt 5027 3)
;; 127035954683

(defn problem062 [] (expt (last (foo 5)) 3))
