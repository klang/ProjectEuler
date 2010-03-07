; A composite is a number containing at least two prime factors. 
; For example, 15 = 3 5; 9 = 3  3; 12 = 2  2  3.

; There are ten composites below thirty containing precisely two, not necessarily distinct, prime factors: 4, 6, 9, 10, 14, 15, 21, 22, 25, 26.

; How many composite integers, n < 10^8, have precisely two, not necessarily distinct, prime factors?

;; the naïve way:
;; (count (filter #(= 2 (count (factors %))) (range 2 (expt 10 3))))
;; 299
;; (count (filter #(= 2 (count (factors %))) (range 2 (expt 10 4))))
;; 2625
;; .. 

;; highest number below 10^8 that has two distinct factors must be below 
;; (/ 2 10^8) = 50000000 < 2 * highest prime below 50000000
;; (factors 49999997)
;; [1181 42337]
;; (factors 49999993)
;; [31 1612903]
;; (factors 49999991)
;; [49999991]
;; this number is the largest prime that can be paired with a number below 10^8
(load "tools")
(defn primes-below [a-big-number a-small-prime]
  (let [limit (floor (+ (/ a-big-number a-small-prime) 1))]
    (take (- a-small-prime 1) (filter prime? (range limit 2 -1)))))

(defn pi [n]
  "number of primes less than or equal to n."
  (count (take-while #(<= % n) primes)))

(defn strange [n]
  (map #(factors %) (range (floor (/ (expt 10 8) n))  (floor (- (/ (expt 10 8) n) 10)) -1)))

;(count (filter #(= 2 (count (factors %))) (range 2 (expt 10 3))))
;(map #(factor %) (filter #(= 2 (count (factors %))) (range 2 (expt 10 3))))
(defn two-factor-integers-below [limit]
  ;; (- (pi (/ limit p)) (pi (- p 1)))
  ;; to solve the actual problem the first run, involves counting the number of primes 
  ;; below about 50 million .. which can not be done with the current version of pi does
  ;; not handle well, as it involves generating just over 5 million primes.
  ;; if these are cached, the rest of the calculations should be fast .. 
  ;; the one minute rule is going to be broken .. find another way to do it.
  )

;;user> (/ (Math/log (expt 10 8)) (Math/log 2))
;;26.5754247590989
;;  (floor (+ (/ (Math/log (expt 10 8)) (Math/log 2)) 1))
;;27
;;user> (take 28 primes)
;;(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97 101 103 107)

(comment
;; user> (/ (Math/log (expt 10 3)) (Math/log 2))
;; 9.965784284662087
;; (floor (+ (/ (Math/log (expt 10 3)) (Math/log 2)) 1))
;; user> (take 11 primes)
;; (2 3 5 7 11 13 17 19 23 29 31)
;; (- (pi (/ limit p)) (pi (- p 1)))
  (+ (- (pi (/ 1000 2)) 0) 
     (- (pi (/ 1000 3)) 1) 
     (- (pi (/ 1000 5)) 2) 
     (- (pi (/ 1000 7)) 3)
     (- (pi (/ 1000 11)) 4)
     (- (pi (/ 1000 13)) 5)
     (- (pi (/ 1000 17)) 6)
     (- (pi (/ 1000 19)) 7)
     (- (pi (/ 1000 23)) 8)		; where are they?
     (- (pi (/ 1000 29)) 9)
     (- (pi (/ 1000 31)) 10)
     ;;(- (pi (/ 1000 37)) 11) ; -2
     )
) 