(ns problem173
  (:use [clojure.math.numeric-tower :only (sqrt abs round)]))

;;  (- (* 4 3) 4)
;;  (- 3 2) => 1 .. an outline of size 1 does not fit here
;;  {:square 3 :laminas 1 :tiles 8}
;;  ###
;;  # #
;;  ###
;;  
;;  (- (* 4 4) 4)
;;  (- 4 2) => 2 .. an outline of size 1 does not fit here
;;  {:square 4 :laminas 1 :tiles 12}
;;  ####
;;  #  #
;;  #  #
;;  ####
;;  
;;  (- (* 4 5) 4)
;;  (- 5 2) => 3 .. all laminas from 3 and below can be added as combinations (1)
;;  {:square 5 :laminas (+ 1 1) :tiles (+ 16 12)}
;;  #####
;;  #   #
;;  #   #
;;  #   #
;;  #####
;;  
;;  (- (* 4 6) 4)
;;  (- 6 2) => 4 .. all laminas from 4 and below can be added as combinations (1)
;;  {:square 6 :laminas (+ 1 1) :tiles (+ 20 8)}
;;  ######
;;  #    #
;;  #    #
;;  #    #
;;  #    #
;;  ######
;;  
(defn indexed [s] (map vector (iterate inc 1) s))
(defn ring [n] 
    (cond (= n 1) 1
          (= n 2) 4
          :else 
          (- (* 4 n) 4)) )
(defn ring [n] (- (* 4 n) 4) )

(comment
  (last (indexed (take-while #(<= % 1000000) (map ring (range 1 300000)))))
  (last (indexed (take-while #(<= % 1000000) (map ring (range 1 300000)))))
  (take 10 (map outline (range 2 300000)))
  ;; => [250001 1000000]
  ;; sides 250001 => 1 million blocks used
)

(defn max-ring-side [tiles]
  ;; (<= (- (* 4 n) 4) 100)
  (first (last (indexed (take-while #(<= % tiles) (map ring (range 1 tiles)))))))

(defn two-rings [n] 
  (+ (ring n) 
     (ring (- n 2))))

(defn 
  m-ring 
  "m rings, including an outline of width n"
  [n m] 
  (- (* m (ring n)) (* 4 m (- m 1))))

(comment
  (= (ring (max-ring-side 100)))
  (ring 26)
  (last (indexed (take-while #(<= % 100) (map ring (range 1 (+ 2 (/ 100 4)))))))

  (last (indexed (take-while #(<= % 100) (map two-ring (range 1 (+ 2 (/ 100 4)))))))
  (max-ring-side (- 100 (ring 14)))
  (max-ring-side (- 100 (ring 15)))

  (defn two-rings [side]
    (= (dec side) (max-ring-side (- 100 (ring side)))))

  (defn first-two-ring-lamina [max-side]
    (inc (count (take-while false? (map two-rings (range 1 max-side))))))
  (first-two-ring-lamina 26)

  (defn two-rings [tiles side]
    (= (dec side) (max-ring-side (- tiles (ring side)))))

  (defn first-two-ring-lamina [tiles max-side]
    (inc (count (take-while false? (map (partial two-rings tiles) (range 1 max-side))))))

  ;;(first-two-ring-lamina 1000000 250001)
  
  (+ (ring 12) (ring 14))
  (+ (ring 11) (ring 13))

  ;; first one 1 tile wide ring = 15
  ;; (4n - 4) + (4 (n - 2) - 4) <= 100
)

;;----

(comment
  (indexed (take-while #(<= % 100) (reductions + (map ring (range 3 27 2)))))
  (indexed (take-while #(<= % 100) (reductions + (map ring (range 5 27 2)))))
  (indexed (take-while #(<= % 100) (reductions + (map ring (range 7 27 2)))))

  (indexed (take-while #(<= % 100) (reductions + (map ring (range 4 27 2)))))
  (indexed (take-while #(<= % 100) (reductions + (map ring (range 6 27 2)))))
  (indexed (take-while #(<= % 100) (reductions + (map ring (range 8 27 2))))))

(defn lamina 
  "how many ring layers is it possible to make, given 'tiles' and the 'maximum ring size', starting from 'start-ring' size"
  [tiles max-ring start-ring]
  (count
   (take-while #(<= % tiles) 
               (reductions + (map ring (range start-ring (inc max-ring) 2))))))

(comment
  (lamina 100 26 4)
  (lamina 100 26 3)
  (lamina 100 26 11)
  (lamina 100 26 12)
  (lamina 100 26 13)
  (lamina 100 26 26)

  (+ 1 (reduce + (map (partial lamina 100 26) (range 3 27))))
  ;; da result
  (reduce + (map (partial lamina 1000000 250001) (range 3 (inc 250001))))
  )

(defn solve173 [tiles max-side]
  (reduce + (map (partial lamina tiles max-side) (range 3 (inc max-side)))))

(= 41 (solve173 100 (max-ring-side 100)))

(defn problem173 [] (solve173 1000000 (max-ring-side 1000000)))

;;1572729



