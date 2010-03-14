(load "tools")

; (distinct (prime-factors 504))
; (2 3 7)
; (reduce * (distinct (prime-factors 504)))
; 42

(sort (take 10 
	    (map #(reduce * (distinct (prime-factors %))) 
		 (iterate inc 2))))

(defn rad [n] (reduce * (distinct (prime-factors n))))
(defn radn [n] {:rad (rad n) :n n})
(defn radn [n] [(reduce * (distinct (prime-factors n))) n])
(def g (into [[1 1]] (map #(radn %) (range 2 11))))
(defn E [n ] (second (nth (sort g) (- n 1))))

(defn rads [limit] (into [[1 1]] (map #(radn %) (range 2 (+ limit 1)))))
(defn E [n limit] (second (nth (sort (rads limit)) (- n 1))))

(def radvector (rads 100000))
(defn E [n radvector] (second (nth (sort radvector) (- n 1))))

;goal, to find E(10000)

(E 10000 radvector)
(time (E 10000 (rads 100000)))
;;"Elapsed time: 25978.545876 msecs"
;;21417
