(ns problem123
  (:use 
   [clojure.test :only (deftest is)]
   [clojure.math.numeric-tower :only (expt)]
   [tools.primes :only (primes)]))

;; Let pn be the nth prime: 2, 3, 5, 7, 11, ..., and let r be the remainder when (pn-1)^n + (pn+1)^n is divided by pn^2.
;; For example, when n = 3, p3 = 5, and 4^3 + 6^3 = 280  5 mod 25.
;; The least value of n for which the remainder first exceeds 10^9 is 7037.
;; Find the least value of n for which the remainder first exceeds 10^10.

(defn euler [p n]
  (rem (+ (expt (- p 1) n) (expt (+ p 1) n)) (* p p)))

(deftest test-euler
  (= (5 (euler 5 3))))

;; problem123> (nth primes 7037)
;; 71069
;; problem123> (euler 71069 7037)
;; 1000225106

;; (count (take-while #(< % (expt 10 5)) (map #(euler %1 %2) primes (iterate inc 1))))
;; problem123> (time (count (take-while #(< % (expt 10 6)) (map #(euler %1 %2) primes (iterate inc 1)))))
;; "Elapsed time: 31.704041 msecs"
;; 278
;; problem123> (time (count (take-while #(< % (expt 10 7)) (map #(euler %1 %2) primes (iterate inc 1)))))
;; "Elapsed time: 595.959463 msecs"
;; 806
;; problem123> (time (count (take-while #(< % (expt 10 8)) (map #(euler %1 %2) primes (iterate inc 1)))))
;; "Elapsed time: 17071.289312 msecs"
;; 2370

;; all counts are off by one, because we do not take the last element, that brings us over the limit

;; just to prove to myself, that the obvious way to do this is just as fast as a loop:

(defn foo [limit]
  (loop [primes primes
	 index 1]
    ;(do  (println (list (first primes) index (euler (first primes) index))))
    (if (< limit (euler (first primes) index))
      index
      (recur (rest primes) (inc index)))))

;; problem123> (time (foo (expt 10 6)))
;; "Elapsed time: 28.846138 msecs"
;; 279
;; problem123> (time (foo (expt 10 7)))
;; "Elapsed time: 586.747687 msecs"
;; 807
;; problem123> (time (foo (expt 10 8)))
;; "Elapsed time: 17155.642511 msecs"
;; 2371

;; but (time (foo (expt 10 9))) takes a looong time
;; obviously brute force is not the way to go here

;; -- well, for every second prime, the remainder seems to reduce to 2 .. less work

(defn foo [limit]
  (loop [primes primes
	 index 1]
    ;(do  (println (list (first primes) index (euler (first primes) index))))
    (if (< limit (euler (first primes) index))
      index
      (recur (rest (rest primes)) (+ index 2)))))

;; problem123> (time (foo (expt 10 7)))
;; "Elapsed time: 314.462429 msecs"
;; 807
;; problem123> (time (foo (expt 10 8)))
;; "Elapsed time: 8588.300133 msecs"
;; 2371

(defn fii [n] (euler (nth primes n) n))

;; problem123> (fii 7037)
;; 1000084366
;; problem123> (fii 7036)
;; 2
;; problem123> (fii 7035)
;; 999293610

;; simpel manual binary search
;;problem123> (fii 21033)
;;9999382662
;;problem123> (fii 21035)
;;10001595590

(defn problem123 [] 0)
