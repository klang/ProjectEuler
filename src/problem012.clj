;; as a function:
(defn triangle [n] (reduce + (range n 0 -1)))

;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55,
;; 1         ; [ 1 2]
;; 1+2       ; [ 3 3]
;; 1+2+3     ; [ 6 4]
;; 1+2+3+4   ; [10 5]
;; 1+2+3+4+5 ; [15 6]

;; as a lazy sequence
(defn triangles []
  (map first (iterate (fn [[a b]] [(+ a b) (inc b)]) [1 2])))

