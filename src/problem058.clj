(ns problem058
  (:use [tools.primes :only (prime?)]))
;Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

;65 64 63 62 61 60 59 58 57
;66 37 36 35 34 33 32 31 56
;67 38 17 16 15 14 13 30 55
;68 39 18  5  4  3 12 29 54
;69 40 19  6  1  2 11 28 53
;70 41 20  7  8  9 10 27 52
;71 42 21 22 23 24 25 26 51
;72 43 44 45 46 47 48 49 50
;73 74 75 76 77 78 79 80 81

;It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13  62%.
;
;If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?

;; just like problem 28 .. but flipped

(def se (map first (iterate (fn [[a b]] [(+ a b) (+ 8 b)]) [1 8])))
(def sw (map first (iterate (fn [[a b]] [(+ a b) (+ 8 b)]) [1 6])))
(def nw (map first (iterate (fn [[a b]] [(+ a b) (+ 8 b)]) [1 4])))
(def ne (map first (iterate (fn [[a b]] [(+ a b) (+ 8 b)]) [1 2])))

(defn f [limit]
  (loop [nep (rest ne) nwp (rest nw) swp (rest sw) sep (rest se) 
	 n 1 t 1 p 0 ]
    (if (and (< 0 p) (<=  (/ p t) limit))
	;;(or (< 21 t) (and (< 0 p) (<=  (/ p t) limit))) ;  (<= (* p 10) t)
      (list n p t (/ p t))
      (recur 
       (rest nep) (rest nwp) (rest swp) (rest sep) 
       (+ n 2) (+ t 4)
       (+ p (reduce + 
		    (map #(if (prime? %) 1 0) 
			 [(first nep) (first nwp) (first swp) (first sep)])))))))

;; user> (time (f 1/10))
;; "Elapsed time: 121704.355926 msecs"
;; (26241 5248 52481 5248/52481)


(comment 
  (do 
    (println 
     (list [(first nep) (first nwp) 
	    (first swp) (first sep) ]
	   [:ps (reduce + 
			(map #(if (prime? %) 1 0) 
			     [(first swp) (first sep) (first nwp) (first nep)]))]
	   [:n n] [:p p] [:t t])))
)

;; supporting function to find out that the initial code was terribly wrong

(defn f4 [n]
     (+ (count (filter prime? (take n sw)))
	(count (filter prime? (take n nw)))
	(count (filter prime? (take n se)))
	(count (filter prime? (take n ne)))))

;; n: 1 3  5  7  9  11  13  15  17
;;   (1 7 21 43 73 111 157 211 273)
;;   (1 9 25 49 81 121 169 225 289)
;;   (1 5 17 37 65 101 145 197 257)
;;   (1 3 13 31 57  91 133 183 241)
;; t: 1 5  9 13 17  21  25  29  33
;; p: 0 3  5  8  9  10  11  13  15

(defn problem058 [] (first (f 1/10)))
