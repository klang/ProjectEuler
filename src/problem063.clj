; one digit 1st powers    [0..9]
(count (map (fn [n] (expt n 1)) (range 0 10)))

; two digit 2nd powers    [4..9]
(count (map (fn [n] (expt n 2)) (range 4 10)))

; three digit 3rd powers  [5..9]
(count (map (fn [n] (expt n 3)) (range 5 10)))

; four digit 4th powers   [6..9]
(count (map (fn [n] (expt n 4)) (range 6 10)))

; four digit 5th powers   [7..9]
(count (map (fn [n] (expt n 5)) (range 7 10)))

;; (count (map (fn [n] (expt n 8)) (range 9 10)))
;; (count (map (fn [n] (expt n 9)) (range 9 10)))
;; (count (map (fn [n] (expt n 10)) (range 8 10)))

; (map #(= % (n-digit (expt 9 %))) (range 11 23))

(defn n-digit [n]
  (.intValue (+ (Math/log10 n) 1)))

(defn n-power-and-n-digit [base n] 
  (= n (n-digit (expt base n))))

(count (filter true? 
	       (for [base (range 0 10) 
		     pow (range 0 22)] 
		 (n-power-and-n-digit base pow))))