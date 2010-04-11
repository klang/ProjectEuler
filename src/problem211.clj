;; For a positive integer n, let σ2(n) be the sum of the squares of its divisors. For example,

;; σ2(10) = 1 + 4 + 25 + 100 = 130.
;; Find the sum of all n, 0 < n < 64,000,000 such that σ2(n) is a perfect square.

;; (custom-set-variables '(swank-clojure-extra-vm-args '("-server" "-Xmx512M")))

(ns p211
  (:use	[tools.primes :only (divisors#)]
	[clojure.contrib.math :only (expt exact-integer-sqrt)]
	[clojure.test]))

;;(- (expt 2 31 ) 1 )

(def perfect-squares (map #(* % %) (iterate inc 1)))

(defn make-square-divs-seq [limit]
  (loop [i (int 0), d (int 2), divs (int-array limit 1)]
    (if (= d limit) 
      divs
      (if (>= i limit) 
	(recur (int 0) (inc d) divs)
	(recur (+ i d) d (do (aset divs i (+ (aget divs i) (* i i))) divs))))))

(defn make-square-divs-transient [limit]
  (loop [i 0 d 2 divs (transient (vec (repeat limit 1)))]
    (if (= d limit) 
      (persistent! divs)
      (if (>= i limit) 
	(recur (int 0) (inc d) divs)
	(recur (+ i d) d (assoc! divs i (+ (divs i) (* i i))))))))

(defn same-cnt [divs]
  (let [divs2 (cons 0 (seq divs))]
    (reduce + (map #(if (= %1 %2) 1 0) divs divs2))))

(defn p211 []
  (let [divs-seq (make-square-divs-seq (int 1e7))]
    (same-cnt divs-seq)))
