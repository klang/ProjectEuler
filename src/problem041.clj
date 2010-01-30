(load "tools")
(use 'clojure.contrib.combinatorics)

(def pandigital (permutations '(1 2 3 4 5 6 7 8 9)))
;; (time (count pandigital))
;; "Elapsed time: 25714.786336 msecs"
;; 362880

;; the easy way to convert back to a number
(defn integer [l] 
  ;; alternatively use BigInteger 
  (Integer. (apply str l)))

(def pandigital (map #(integer %) (permutations '(1 2 3 4 5 6 7 8 9))))
;; (time (count pandigital))
;; "Elapsed time: 17821.7888 msecs"
;; 362880

(def odd-pandigital (filter #(odd? %) pandigital))
;; (time (count odd-pandigital))
;; "Elapsed time: 34242.935146 msecs"
;; 201600

;;  (time (last (filter #(prime? %) odd-pandigital)))
;; "Elapsed time: 1259.126597 msecs"
;; nil

(defn pandigital [n] (map #(integer %) (permutations (range 1 (+ 1 n)))))

;; no pandigital primes of size 9 .. 
;; (time (last (filter #(prime? %) (pandigital 8))))
;; "Elapsed time: 1556.962586 msecs"

;; (time (last (filter #(prime? %) (pandigital 7))))
;; Evaluation aborted.

;; as we are looking for the largest pandigital prime, we reverse the range
(defn pandigital [n] (map #(integer %) (permutations (range n 0 -1))))
;;
;;(time (first (filter #(prime? %) (pandigital 7))))
;; Evaluation aborted.
(def pandigital7 (filter #(odd? %) (pandigital 7)))
;;user> (count (pandigital 7))
;;5040
;;(time (first (filter #(prime? %) pandigital7)))
; Evaluation aborted.

;; (filter #(prime? %) (take 7 pandigital7))
;;()
;; (filter #(prime? %) (take 8 pandigital7))
; Evaluation aborted.
;; (take 8 pandigital7)
;;(7654321 7654231 7654213 7654123 7653421 7653241 7652431 7652413)

;; --> probably 7652413

;; (time (prime-factors 7652413))
;; "Elapsed time: 11.263166 msecs"
;; [7652413]

;; (time (factors 7652413))
;; "Elapsed time: 6208.94227 msecs"
;; [7652413]

;; with the better algorithm for prime checking, thing go a lot better
; (time (first (filter #(prime? %) pandigital7)))
;; "Elapsed time: 640.775216 msecs"
;; 7652413
