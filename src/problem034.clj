(load "tools")

(reduce + (map #(factorial %) (digits 145)))

(defn matches-sum-of-facforials? [n]
  (= n (reduce + (map #(factorial %) (digits n)))))

;; (filter #(matches-sum-of-facforials? %) (range 3 1000000))
;; (145 40585)
;; let us just try
;; user> (+ 145 40585)
;; 40730

;; it works, but a limiting function should be possible to make
