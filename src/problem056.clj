;;A googol (10^100) is a massive number: one followed by one-hundred zeros; 100100 is almost unimaginably large: one followed by two-hundred zeros. Despite their size, the sum of the digits in each number is only 1.

;; Considering natural numbers of the form, ab, where a, b  100, what is the maximum digital sum?

; (reduce + (digits (expt 100 100)))
; oh, well .. that number was bigger than the one in the example

(def p056 
     (map (fn [[a b]] [(reduce + (digits (expt a b))) (list a b)]) 
	  (for [a (range 0 100) b (range 0 100)] [a b])))

(defn max-p056 []
  (loop [current-max 0
	 max-values []
	 e p056]
    (if (='() e)
      (list current-max max-values)
      (if (< current-max (first (first e)))
	(recur (first (first e)) (second (first e)) (rest e))
	(recur current-max max-values (rest e))))))