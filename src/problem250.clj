;; Find the number of non-empty subsets of {1^1, 2^2, 3^3,..., 250250^250250}, the sum of whose elements is divisible by 250. Enter the rightmost 16 digits as your answer.

;; http://en.wikipedia.org/wiki/Stirling_number

;; user> (factors 250250)
;; [2 5 5 5 7 11 13]
;; user> (factors 250)
;; [2 5 5 5]
;; user> (rem 250250 250)
;; 0
;; user> (factors (expt 250 2))
;; [2 2 5 5 5 5 5 5]


;; user> (rem (expt 250250 250250) 250)
;; 0

;; user> (filter #(< 0 %) (take 250 (map #(if (= 0 (rem (expt % %) 250)) % 0)  (iterate inc 1))))
;; (10 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 250)

; {10} .. 1 subset with 1 member
; {10,20}, {10, 30} .. {10,250}         .. 250-1 subsets with 2 members
; {10,20,30}, {10,20,40} .. {10,20,250}

;  n possible members
;; n   subsets with n - (n-1) = 1 member
;; n-1 subsets with n - (n-2) = 2 members
;; n-2 subsets with n - (n-3) = 3 members
;; .. 
;; 1 subset with n - 1 members 

; 1 + 2 + 3 + .. 250250
;user> (reduce + (range 1 250251))
; 31312656375


;; the n's giving the numbers divisible by 250
(reduce + (range 10 (+ 250250 10) 10))

;; the result for a small range (enough to get the 16 first digits)
(reduce + (map #(expt % %) (range 10 40 10)))
;; ......0000010000000000



(reduce + 
	(filter #(< 0 %) 
		(take 250 
		      (map #(if (= 0 (rem (expt % %) 250)) 
			      (expt % %) 
			      0)  
			   (iterate inc 1)))))

;;; ------
;; user> (reduce + (range 1 250251))
;; 31312656375
;; wrong

;; user> (apply str (take-last 16 (digits (reduce + (filter #(< 0 %) (take 250 (map #(if (= 0 (rem (expt % %) 250)) (expt % %) 0)  (iterate inc 1))))))))
;; "0000010000000000"
;; wrong

;; (apply str (take-last 16 (digits (- (expt 2 250250) 1))))
;; "3326753402650623"
;; wrong