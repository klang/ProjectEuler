(load "problem045")
;; gives lazy sequences:
;;  triangles, pentagonals, hexagonals
;; and functions:
;;  triangle, pentagonal, hexagonal

;; lazy sequences
(def square 
  (map (fn [n] (* n n)) (iterate inc 1)))
(def heptagonals 
  (map (fn [n] (quot (* n (- (* 5 n) 1)) 3)) (iterate inc 1)))
(def octagonals 
  (map (fn [n] (* n (- (* 3 n) 2))) (iterate inc 1)))

;; as functions
(defn square [n] (* n n))
(defn heptagonal [n] (quot (* n (- (* 5 n) 3)) 2))
(defn octagonal [n] (* n (- (* 3 n) 2)))
