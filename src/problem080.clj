(load "tools")
(load "problem064")
(load "problem065")

(defn count-odd-periods-b [N]
  (count (filter odd-period? 
		 (map #(contiuned-fraction-for-sqrts %) 
		      (take-while #(<= % N) not-perfect-squares)))))
