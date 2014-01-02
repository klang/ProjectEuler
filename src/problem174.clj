(ns problem174
  (:use [clojure.math.numeric-tower :only (sqrt abs round)]))

(defn indexed [s] (map vector (iterate inc 1) s))

(defn ring [n] (- (* 4 n) 4) )

(defn max-ring-side [tiles]
  ;; (<= (- (* 4 n) 4) 100)
  (first (last (indexed (take-while #(<= % tiles) (map ring (range 1 tiles)))))))

(defn laminas [tiles max-ring start-ring]
  (take-while #(<= % tiles) 
              (reductions + (map ring (range start-ring (inc max-ring) 2)))))

;; number of different combinations
;; (reduce + (vals (frequencies (flatten (map (partial laminas 100 26) (range 3 27))))))

(map first (filter #(= 1 (second %)) 
                   {32 2, 64 2, 96 4, 36 1, 68 1, 100 1, 8 1, 40 2, 72 3, 12 1, 44 1, 76 1, 16 1, 48 3, 
                    80 3, 20 1, 52 1, 84 2, 24 2, 56 2, 88 2, 28 1, 60 2, 92 1}))
(defn L-map [tiles]
  (let [max-side (max-ring-side tiles)]
    (frequencies (flatten (map (partial laminas tiles max-side) (range 3 (inc max-side)))))))

(defn L [pre-calc-map n]
  (count (map first (filter #(= n (second %)) pre-calc-map))))

;; (= 832 ((partial L (L-map 1000000)) 15))

(defn problem174 [] (reduce + (map (partial L (L-map 1000000)) (range 1 11))))
;; (time (problem174))
;; "Elapsed time: 6752.041 msecs" 
;; MacBook Pro, Early 2013
;;209566







