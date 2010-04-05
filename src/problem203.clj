;; http://www.research.att.com/~njas/sequences/A000984

;; C(2n,n) = (2n)!/(n!)^2.
(defn C [n]
  (/ (factorial (* 2 n)) (expt (factorial n) 2)))

;It can indeed be solved in one equation, and curiously enough, the solution to it is in one of the other problems. The solution lies in the N choose R formula. For any number of rows R, you take (2R!)/(R! ^ 2). This comes from the equation n choose r = n!/r!(n-r)!, and you replace n with 2r, for the number of rows problem. :) 

;P.S. I liked the hint "rth element in the nth row"! ;)

;; from http://rosettacode.org/wiki/Pascal's_triangle#Clojure

(defn nextrow [row]
  (vec (concat [1] (map #(apply + %) (partition 2 1 row)) [1] )))
 
(defn pascal [n]
  (assert (and (integer? n) (pos? n)))
  (let [triangle (take n (iterate nextrow [1]))]
    (doseq [row triangle]
      (println row))))

(def the51st [1 50 1225 19600 230300 2118760 15890700 99884400 536878650 2505433700 10272278170 37353738800 121399651100 354860518600 937845656300 2250829575120 4923689695575 9847379391150 18053528883775 30405943383200 47129212243960 67327446062800 88749815264600 108043253365600 121548660036300 126410606437752 121548660036300 108043253365600 88749815264600 67327446062800 47129212243960 30405943383200 18053528883775 9847379391150 4923689695575 2250829575120 937845656300 354860518600 121399651100 37353738800 10272278170 2505433700 536878650 99884400 15890700 2118760 230300 19600 1225 50 1])

;; (use '[clojure.contrib.math :only (expt exact-integer-sqrt)])
;; (exact-integer-sqrt 126410606437752)
;; user> (exact-integer-sqrt 126410606437752)
;; [11243247 3334743]
;; user> (exact-integer-sqrt 11243247)
;; [3353 638]
