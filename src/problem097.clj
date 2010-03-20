;; The first known prime found to exceed one million digits was discovered in 1999, and is a Mersenne prime of the form 2^6972593 - 1; it contains exactly 2,098,960 digits. Subsequently other Mersenne primes, of the form 2^p - 1, have been found which contain more digits.
;; 
;; However, in 2004 there was found a massive non-Mersenne prime which contains 2,357,207 digits: 28433 * 2^7830457 + 1.
;; 
;; Find the last ten digits of this prime number.

(deftest test-calc
  (is (= (* (expt 2 1000) (expt 2 234))
	 (expt 2 1234)))
  (is (= (mod (expt 2 1234) (expt 10 10))
	 (mod (* (mod (expt 2 234) (expt 10 10)) 
		 (mod (expt 2 1000) (expt 10 10))) (expt 10 10))))
  (is (= (mod (expt 2 1234) (expt 10 10))
	 (mod (reduce * (map #(mod % (expt 10 10)) (map #(expt 2 %) [234 500 500]))) (expt 10 10))))
  (is (= (* 107 (expt 2 647))
	 (* 107 
	    (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
	    (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
	    (expt 2 47)))))

(defn lsd [n number]
  "the n least significant digits of number"
  (mod number (expt 10 n)))

(defn lsd-expt [n base pow]
  "returns the n least significant digits of expt base pow.
  divides pow in cycles of size divider and keeps the calculations lower than (expt base divider)"
  (let [divider   50
	cycles    (quot pow divider)
	remainder (rem pow divider)]
    (lsd n
	 (* (lsd n (expt base remainder)) 
	 (lsd n (reduce * (map #(lsd n %) 
;			       (map #(expt base %) (take divider (cycle [cycles])))
			       (take divider (cycle [(lsd n (expt base cycles))]))
			       )))))))

;; not fast enough for very high numbers (with the map calculation on each element)

;; simply calculating the cycle part once reduces the execution time significantly
;; user> (time (+ (lsd-mult 10 28433 (lsd-expt 10 2 7830457)) 1))
;; "Elapsed time: 313.260038 msecs"
;; 8739992577

;; (whild goose chacing by making lsd-mult and lsd)

(deftest test-lsd-expt
  (is (= (lsd 10 (expt 2 100))
	 (lsd-expt 10 2 100))
      (= (lsd 10 (expt 2 101))
	 (lsd-expt 10 2 101)))
  (is (= (lsd 10 (* 107 (expt 2 647)))
	 (lsd 10 (* 107 (lsd-expt 10 2 647)))
	 (lsd 10 (* 107 
		    (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
		    (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
		    (expt 2 47))))))


(defn lsd-mult 
  "least significant digit multiplication (modulo multiplication)"
  ([n] 1) 
  ([n x] (lsd n x))
  ([n x y] (lsd n (* (lsd n x) (lsd n y))))
  ([n x y & more]
     (lsd n (reduce * (lsd-mult n x y) more))))

(deftest test-lsd-mult
  (is (= (lsd 10 (* 107 (expt 2 647)))
	 (lsd 10 (* 107 
		    (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
		    (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
		    (expt 2 47)))
	 (lsd-mult 10 107 
		   (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
		   (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
		   (expt 2 47))))
  (is (= (lsd 10 (* (expt 2 50) (expt 2 50)))
       (lsd-mult 10 (expt 2 50) (expt 2 50))))
  (is (= (lsd 10 (* 107 (expt 2 50) (expt 2 50)))
	 (lsd-mult 10 107 (expt 2 50) (expt 2 50))))
  (is (= (lsd 10 (* 107 (expt 2 50) (expt 2 50)))
	 (lsd-mult 10 107 (expt 2 50) (expt 2 50))))
  (is (= (lsd 10 (* 107 (expt 2 647)))
	 (lsd 10 (* 107 
		    (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
		    (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50) (expt 2 50)
		    (expt 2 47))))))

(defn lsd-expt2 [n base pow]
  "returns the n least significant digits of expt base pow.
  divides pow in cycles of size divider and keeps the calculations lower than (expt base divider)"
  (let [divider   375
	cycles    (quot pow divider)
	remainder (rem pow divider)]
    (lsd n 
	 (lsd-mult n (expt base remainder)
		   (reduce (fn [x y] (lsd-mult n x y)) 
			   ;(map #(expt base %) (take divider (cycle [cycles])))
			   (take divider (cycle [(lsd n (expt base cycles))]))
			   )
		   ))))

(deftest test-lsd-expt2
  (is (= (lsd-mult 10 107 (lsd-expt2 10 2 647))
	 (lsd-mult 10 107 (lsd-expt2 10 2 600)  (expt 2 47)))))


;; 10 least significant digits of 28433 * 2^7830457 + 1

;; divider 375
;; user> (+ (lsd-mult 10 28433 (lsd-expt2 10 2 7830457)) 1)
;; 8739992577

(load "helpers")
;; (+ (mod (* 28433 (exptm 2 7830457 (expt 10 10)))(expt 10 10)) 1)
;; 8739992577

