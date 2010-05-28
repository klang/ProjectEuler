(ns problem061
  (:use tools.numbers)
  (:use problem045))
;; gives lazy sequences:
;;  triangles, pentagonals, hexagonals
;; and functions:
;;  triangle, pentagonal, hexagonal

;; lazy sequences
(def squares 
  (map (fn [n] (* n n)) (iterate inc 1)))
(def heptagonals 
  (map (fn [n] (quot (* n (- (* 5 n) 1)) 3)) (iterate inc 1)))
(def octagonals 
  (map (fn [n] (* n (- (* 3 n) 2))) (iterate inc 1)))

;; as functions
(defn square [n] (* n n))
(defn heptagonal [n] (quot (* n (- (* 5 n) 3)) 2))
(defn octagonal [n] (* n (- (* 3 n) 2)))

(def tria (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) triangles))))
(def squa (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) squares))))
(def pent (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) pentagonals))))
(def hexa (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) hexagonals))))
(def hept (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) heptagonals))))
(def octa (into #{} (take-while #(< % 10000) (drop-while #(<= % 1000) octagonals))))

(comment
  ;; there are not that many 4 digit numbers
  (map #(count %) [tria squa pent hexa hept octa])
)

(comment
  ;; for each element (octa is the smallest group) check two different groups for fronts and backs
  ;; .. a lot of different combinations .. 
  (map #(hash-map :front (quot % 100) :back (rem % 100)) octa)
)