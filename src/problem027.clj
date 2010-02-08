(load "tools")
(use 'clojure.contrib.test-is)
;; Euler published the remarkable quadratic formula:

;; n² + n + 41

;; It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

;; Using computers, the incredible formula  n² - 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. The product of the coefficients, 79 and 1601, is 126479.

;; Considering quadratics of the form:

;;n² + an + b, where |a| < 1000 and |b| <  1000

;; where |n| is the modulus/absolute value of n, e.g. |11| = 11 and |4| = 4

;; Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.

;; a function that produces functions of the needed form
(defn f [a b]
  (fn [n] (+ (* n n) (* a n ) b)))


(defn count-euler027 [a b]
  (count (take-while prime? (map #((f a b) %) (iterate inc 0)))))

(deftest test-euler027
    (is (= 40 (count-euler027 1 41)))
    (is (= 80 (count-euler027 -79 1601))))

; (for [a (range -999 1000) b (range -999 1000)] [a b])

; (for [a (range -1 2) b (range -1 2)] [a b])

(defn values-euler027 [r]
  (map (fn [[a b]] [(count-euler027 a b) (list a b)]) 
       (for [a (range (- 1 r) (+ r 1)) 
	     b (range (- 1 r) (+ r 1))] [a b])))

(defn ranges [r]
  (list (- 1 r) (+ r 1)))

(defn max-euler027 [r]
  (loop [current-max 0
	 max-values  []
	 e (values-euler027 r)]
      (if (= '() e)
	(list current-max max-values)
	(if (< current-max (first (first e)))
	  (recur (first (first e)) (second (first e)) (rest e))
	  (recur current-max max-values (rest e)))
	)
      )
  )
;; user> (time (max-euler027 41))
;; "Elapsed time: 171.011185 msecs"
;; (41 (-1 41))
;; user> (time (max-euler027 1000))
;; "Elapsed time: 100381.217611 msecs"
;; (71 (-61 971))
;; (* -61 971)

;; optimazions .. Taking hints from the forum
;; As well as the fact that b must be prime, note that a > -b as: 
;; f(1) is prime 
;; => f(1) > 1 
;; => 1 + a + b > 1 
;; => a > -b 

;; so loop b through the primes less than 1000, and within that loop, loop a from -b to 1000

;; redefine the function that returns the value counts
(defn values-euler027 [r]
  (map (fn [[a b]] [(count-euler027 a b) (list a b)]) 
       (for [b (take-while #(< % r) primes) 
       a (range (- b) r)] 
   [a b])))

;; user> (time (max-euler027 1000))
;; "Elapsed time: 17856.099894 msecs"
;; (71 (-61 971))

