(use 'clojure.contrib.math)

;; http://en.wikipedia.org/wiki/Continued_fraction
;; convert number to continued fraction representation
(defn continued-fraction [number]
  "calculating a continued fraction based on http://en.wikipedia.org/wiki/Continued_fraction#Calculating_continued_fraction_representations"
  (let [num (rationalize number)]
    (if (not (ratio? num)) 
      [num]
      (loop [n (.numerator num), d (.denominator num)
	     r (quot n d), f (- n (* r d))
	     cont-frac [r]]
	(if (zero? f)
	  cont-frac
	  (let [r1 (quot d f), f1 (- d (* r1 f))]
	    (recur  d f r1 f1 (conj cont-frac r1))))))))

; [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...]
; first 10
;2, 3, 8/3, 11/4, 19/7, 87/32, 106/39, 193/71, 1264/465, 1457/536
;(take 10 (map (fn [k] [1 (* 2 k) 1]) (iterate inc 1)))
(reduce concat (take 10 (map #(vector 1 (* 2 %) 1) (iterate inc 1))))

(def continued-fraction-e 
     (interleave (cycle [1]) (map #(* 2 %) (iterate inc 1)) (cycle [1])))

;; lazy sequence that first returns 2, then elements from continued-fraction-e
;; 2
;; 2 + 1
;; 2 + 1 +
(def e )

