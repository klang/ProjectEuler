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

;(continued-fraction 415/93)
;[4 2 6 7]
;(+ 4 (/ 1 (+ 2 (/ 1 (+ 6 (/ 1 7))))))

; [2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...]
;(take 10 (map (fn [k] [1 (* 2 k) 1]) (iterate inc 1)))
;(reduce concat (take 10 (map #(vector 1 (* 2 %) 1) (iterate inc 1))))

;; Returns a lazy sequence of the continued fraction representation of e. 
;; e = (2; 1,2,1, 1,4,1, 1,6,1 , ... , 1,2k,1, ...)"
(def continued-fraction-e 
     (lazy-cat '(2) 
	       (interleave (cycle [1]) 
			   (map #(* 2 %) (iterate inc 1)) 
			   (cycle [1]))))
;(take 30 continued-fraction-e)

; first 10
; (2 1 2 1 1 4 1 1 6 1)
; (2 3 8/3 11/4 19/7 87/32 106/39 193/71 1264/465 1457/536)
;; (+ 2)
;; (+ 2 (/ 1 1))
;; (+ 2 (/ 1 (+ 1 (/ 1 2))))
;; (+ 2 (/ 1 (+ 1 (/ 1 (+ 2 (/ 1 1))))))
;; (+ 2 (/ 1 (+ 1 (/ 1 (+ 2 (/ 1 (+ 1 (/ 1 1))))))))
;; (+ 2 (/ 1 (+ 1 (/ 1 (+ 2 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 4))))))))))
;; (+ 2 (/ 1 (+ 1 (/ 1 (+ 2 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 4 (/ 1 1))))))))))))
;; (+ 2 (/ 1 (+ 1 (/ 1 (+ 2 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 4 (/ 1 (+ 1 (/ 1 1))))))))))))))
;; (+ 2 (/ 1 (+ 1 (/ 1 (+ 2 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 (+ 4 (/ 1 (+ 1 (/ 1 (+ 1 (/ 1 6))))))))))))))))
;; 
;(+ 4 (/ 1 (+ 2 (/ 1 (+ 6 (/ 1 7))))))
;; function that returns a certain number of estimations of a cf representation
(defn estimate-continued-fraction [fraction]
  (+ (first fraction) 
     (if (= '() (rest fraction))
       0
       (/ (estimate-continued-fraction (rest fraction))))))

;; user> (estimate-continued-fraction [4 2 6 7])
;; 415/93
;; user> (estimate-continued-fraction (take 10 continued-fraction-e))
;; 1457/536

;; user> (estimate-continued-fraction (take 100 continued-fraction-e))
;; 6963524437876961749120273824619538346438023188214475670667/2561737478789858711161539537921323010415623148113041714756
;; (load "tools")
;; user> (reduce + (digits (.numerator (estimate-continued-fraction (take 100 continued-fraction-e)))))
;; 272
;; 
