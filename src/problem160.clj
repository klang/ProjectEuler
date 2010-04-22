(ns problem160
  (:use tools.numbers)
  (:use tools.primes)
  (:use clojure.contrib.math)
  (:use clojure.test))

;; http://en.wikipedia.org/wiki/Factorial
;; The asymptotically-best efficiency is obtained by computing n! from its prime factorization.

;; factorial is defined this way, in tools.numbers
;;(defn factorial [n] (reduce * (range n 0 -1)))
;; definately not the correct way to do this, for large numbers.
;; But it gives us the principle.

(defn f [n]
  (integer (reverse 
	    (take 5 
		  (drop-while zero? 
			      (reverse 
			       (digits (factorial n))))))))

(deftest test-f
  (is (= 36288 (f 9)))
  (is (= 36288 (f 10)))
  (is (= 17664 (f 20))))

;; problem160> (factors (expt 10 12))
;; [2 2 2 2 2 2 2 2 2 2 2 2 5 5 5 5 5 5 5 5 5 5 5 5]
;; (* (expt 2 12) (expt 5 12))
;; 1000000000000

;; http://www.luschny.de/math/factorial/FactorialDigits.html
(defn ln [n] (Math/log n))
(def pi (Math/PI))

;b:=2*10^n
;
;ceil(((n*(b+1)+(ln(2*Pi)-b+1/b)/ln(10)))/2)
(defn factorial-digits [n]
  (cond (zero? n) 1
	:else
	(let [b (* 2 (expt 10 n))
	      c (* 1/2 n (+ 1 b))
	      g (- (ln (* 2 pi)) (+ b (/ 1 b)))
	      h (* 1/2 (/ g (ln 10)))]
	  (int (ceil (+ c h))))))

;;problem160> (factorial-digits 12)
;;2147483647

;; http://en.wikipedia.org/wiki/Trailing_zeros#Factorial
(defn trailing-zeroes [n]
  (reduce + (map #(floor (/ n (expt 5 %))) 
		 (take-while #(>= n (expt 5 %)) 
			     (iterate inc 1)))))

;; problem160> (trailing-zeroes (expt 10 12))
;; 249999999997

(defn f1 [n]
  (let [a (trailing-zeroes n)
	b1 (expt 10 a)
	b2 (* (expt 10 5) b1)]
    (quot (mod (factorial n) b2) b1)))

;; f1 is much faster than f, but the expensive factorial operation is still done

(deftest test-f1
  (is (= 36288 (f1 9)))
  (is (= 36288 (f1 10)))
  (is (= 17664 (f1 20))))


;; http://www.luschny.de/math/factorial/FastFactorialFunctions.htm

;; problem160> (time (f1 10000))
;; "Elapsed time: 18271.535222 msecs"
;; 79008

(defn special-factorial [n] 
  (reduce * (range n 0 -1)))

(defn s [next so-far] (mod (* next so-far) (expt 10 5)))

;; problem160> (time (f2 100000))
;; "Elapsed time: 0.29473 msecs"
;; 62496
;; problem160> (time (f2 1000000))
;; "Elapsed time: 2244.540732 msecs"
;; 62496
;; problem160> (time (f2 10000000))
;; "Elapsed time: 28711.281465 msecs"
;; 62496
;; not the correct answer..

(defn f2 [n]
  (let [ex (expt 10 5)]
    (loop [current (range 10 (+ n 1))
	   ;current (range 100000 (+ n 1))
	   ;so-far 62496
	   so-far 36288
	   ]
      (if (empty? current)
	so-far
	(let [candidate (mod (* (first current) so-far) ex)
	      check (mod candidate 10)]
	  (recur (rest current) (if (zero? check) so-far candidate)))))))

;; under an int
;; (last (take-while #(< (factorial %) 2147483647) (iterate inc 1)))

;; (* (*  (* 399168 12) 13) 14)
;; (* (* (*  (* 399168 12) 13) 14) 15)
;; 13076743680
;; (* 1307674368 16)
;; 20922789888
;; (factorial 16)
;; 20922789888000
;; 

;--- 
;; problem160> (type (* (* (*  (* 399168 12) 13) 14)))
;; java.lang.Integer

;(defn factorial [n] (reduce * (range n 0 -1)))
(defn f3 [n]
  (letfn [(fac [n] (reduce * (range 1 (+ n 1))))]
    (integer (reverse 
	      (take 5 
		    (drop-while zero? 
				(reverse 
				 (digits (fac n)))))))))

(defn f3partial [start end]
  (letfn [(fac [start end] (reduce * (range start (+ end 1))))]
    (integer (reverse 
	      (take 5 
		    (drop-while zero? 
				(reverse 
				 (digits (fac start end)))))))))
(defn f3p [n]
  (f3partial 1 n))

(defn interesting [n number]
  (integer (reverse (take n (drop-while zero? (reverse (digits number)))))))

(defn fac-partial [start end] (reduce * (range start (+ end 1))))

(deftest test-f3partial
  (is (=  (interesting 5 (* (f3partial 1 500) (f3partial 501 1000)))
	  (f3partial 1 1000)))
  (is (=  (interesting 5 (* (f3partial 1 500) (f3partial 501 1000) (f3partial 1001 10000)))
	  (f3partial 1 10000)))
  (is (= (* (fac-partial 1 10) (fac-partial 11 15)) (fac-partial 1 15) (factorial 15))))


(defn f4 [n]
  (loop [current (range 1 (+ n 1)) so-far 1]
    (cond 
      (zero? (mod so-far 10)) (recur current (quot so-far 10)) ;; knock down result if there are trailing zeroes
      (empty? current) (mod so-far (expt 10 5))
      (zero? (mod (first current) 10)) (recur (rest current) (* (quot (first current) 10) so-far))
      :else
      (recur (rest current) (* (first current) so-far))
      )))

(deftest test-f4
  (is (= 36288 (f4 9)))
  (is (= 36288 (f4 10)))
  (is (= 17664 (f4 20))))

(defn f5 [n]
  (let [ex (expt 10 12)]
    (loop [current (range 1 (+ n 1)) so-far 1]
      (cond 
	(zero? (mod so-far 10)) (recur current (quot so-far 10)) ;; knock down result if there are trailing zeroes
	(empty? current) (mod so-far (expt 10 5))
	(zero? (mod (first current) 10)) (recur (rest current) (* (quot (first current) 10) (mod so-far ex)))
	:else
	(recur (rest current) (* (first current) (mod so-far ex)))
	))))

(deftest test-f5
  (is (= 36288 (f5 9)))
  (is (= 36288 (f5 10)))
  (is (= 17664 (f5 20))))

(deftest test-f
  (is (every? true? (map #(= (f %) 
			     (f1 %) 
			     (f3 %) 
			     (f4 %) 
			     (f5 %)) (range 1 20)))))



;; problem160> (time (f5 (expt 10 7)))
;; "Elapsed time: 50709.247663 msecs"
;; 94688
;; problem160> (time (count (range 1 (expt 10 7))))
;; "Elapsed time: 7301.081969 msecs"
;; 9999999


;(map #(prime-factors %) (range 2 21))

;(* 2 3 2 2 5 2 3 7 2 2 2 3 3 2 5 11 2 2 3 13 2 7 3 5 2 2 2 2 17 2 3 3 19 2 2 5)
;(sort'(2 3 2 2 5 2 3 7 2 2 2 3 3 2 5 11 2 2 3 13 2 7 3 5 2 2 2 2 17 2 3 3 19 2 2 5))
;(2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 5 5 5 5 7 7 11 13 17 19)
;(* (expt 2 18) (expt 3 8) (expt 5 4) (expt 7 2) 11 13 17 19) 

;; problem160> (* (expt 2 18) (expt 3 8) (expt 5 4) (expt 7 2) 11 13 17 19) 
;; 2432902008176640000
;; (mod (* (expt 2 x) (expt 5 x)) 10) = 0 =>  
;; problem160> (* (expt 2 14) (expt 3 8) (expt 7 2) 11 13 17 19) 
;; 243290200817664


;; we only have to factor until a certain limited point
(comment
  (= 27296 (f5 30000))
  (= 20096 (f5 60000) (f5 300000))
  (= 23264 (f5 70000))
  (= 15776 (f5 90000))
  (= 20736 (f5 600000) (f5 3000000))
  (= 70112 (f5 700000))
  (= 84736 (f5 900000))
  (= 92576 (f5 6000000))
  (= 92576 (mod (f9 100000) (f9s 100000 200000) (f9s 1000000 1200000) (expt 10 5)))
  (= 98656 (f5 7000000))
  (= 98656 (mod (* 12544 (f9s 1000000 1400000)) (expt 10 5)))
  (= 54176 (mod (* (f5 100000)  (f9s 100000 160000)) (expt 10 5)))
  (= 38144 (mod (* (f5 1000000) (f9s 1000000 1600000)) (expt 10 5)))

  (= 88096 (f5 9000000))
  (= 88096 (mod (* 12544 (f9s 1000000 1800000)) (expt 10 5)))
  (= 94688 (rem (* 88096 (f9s 9000001 10000000)) (expt 10 5)))
  (= 54176 (rem (* 62496 (f9s 500001 800000)) (expt 10 5))) 
  ;; (* (fs 1 5000000) (fs 5000001 8000000)) = (fs 1 800000)
  (= 67808 (rem (* 54176 (f9s 500000001 800000000)) (expt 10 5))) 
  (= 67808 (rem (* 54176 89408) (expt 10 5))) 
  ;; (* (f9 1 500000000) (f9s 500000001 800000000)) = (f9s 1 80000000)
  (= res   (rem (* 67808 (f9s 5000000001 8000000000)) (expt 10 5))) 
  ;; (* (f9 1 500000000) (f9s 5000000001 8000000000)) = (f9s 1 800000000)

  (= 79008                                                    (f5 10000)        (f5 50000))
  (= 62496                                  (f5 20000)        (f5 100000)       (f5 500000))
  (= 12544                 (f5 40000)       (f5 200000)       (f5 1000000)      (f5 5000000))
  (= 94688 (f5 80000)      (f5 400000)      (f5 2000000)      (f5 10000000)     (f5 50000000))
  (= 54176 (f5 800000)     (f5 4000000)     (f5 20000000)     (f5 100000000)    (f? 500000000))
  (= 38144 (f5 8000000)    (f? 40000000)    (f? 200000000)    (f? 1000000000)   (f? 5000000000))
  (= 46112 (f5 80000000)   (f? 400000000)   (f? 2000000000)   (f? 10000000000)  (f? 50000000000))
  (= 67808 (f? 800000000)  (f? 4000000000)  (f? 20000000000)  (f? 100000000000) (f? 500000000000))
  (= 0     (f? 8000000000) (f? 40000000000) (f? 200000000000) (f? 1000000000000))
)

(defn f6 [n]
  "returns the interesting digits and the number of different possible digits on the way to the result"
  (let [ex (expt 10 12)]
    (loop [current (range 1 (+ n 1)) so-far 1 catch #{}]
      (cond 
	(zero? (mod so-far 10)) (recur current (quot so-far 10) catch) ;; knock down result if there are trailing zeroes
	(empty? current) (list (mod so-far (expt 10 5)) (count catch))
	(zero? (mod (first current) 10)) (recur (rest current) (* (quot (first current) 10) (mod so-far ex)) catch)
	:else
	(recur (rest current) (* (first current) (mod so-far ex)) (conj catch (mod so-far (expt 10 5))))))))

(comment
  (reduce into [] (map #(prime-factors %) (range 2 21)))
  (def p (reduce into [] (map #(prime-factors %) (range 2 10001))))
  (count (filter #(= 2 %) (sort p))))

;; using "normal" count instead of range reduces the execution time by 50% (compare with f5
(defn f7 [n]
  (let [ex (expt 10 12)]
    (loop [current 1 so-far 1]
      (cond 
	(zero? (mod so-far 10)) (recur current (quot so-far 10))
	(< n current) (mod so-far (expt 10 5))
	(zero? (mod current 10)) (recur (+ 1 current) (* (quot current 10) (mod so-far ex)))
	:else
	(recur (+ 1 current) (* current (mod so-far ex)))))))

;; one of the branches is really not needed
(defn f8 [n]
  (let [ex (expt 10 12)]
    (loop [current 1 so-far 1]
      (cond 
	(zero? (mod so-far 10)) (recur current (quot so-far 10))
	(< n current) (mod so-far (expt 10 5))
	:else
	(recur (+ 1 current) (* current (mod so-far ex)))))))

;; rem is very much faster than mod .. 
(defn f9 [n]
  (let [ex (expt 10 12)]
    (loop [current 1 so-far 1]
      (cond 
	(zero? (rem so-far 10)) (recur current (quot so-far 10))
	(< n current) (rem so-far (expt 10 5))
	:else
	(recur (+ 1 current) (* current (rem so-far ex)))))))

(defn f9s [start end]
  (let [ex (expt 10 12)]
    (loop [current start so-far 1]
      (cond 
	(zero? (rem so-far 10)) (recur current (quot so-far 10))
	(< end current) (rem so-far (expt 10 5))
	:else
	(recur (+ 1 current) (* current (rem so-far ex)))))))

(comment
  (= (f9s 1 1000000) 
     (rem (* (rem (* (f9s 1 10000) (f9s 10000 20000)) (expt 10 5)) 
	     (f9s 100000 200000)) (expt 10 5)))

  (= 12544 ;;(f9s 1 1000000) 
     (rem (* (rem (* (f9s 1 10000) (f9s 10000 20000)) (expt 10 5)) 
	     (f9s 100000 200000)) (expt 10 5)))

  (= 12544 ;;(f9s 1 1000000) 
     (rem  (* (f9s 1 10000) (f9s 10000 20000) (f9s 100000 200000) ) (expt 10 5)))
  (= 94688 ;;(f9s 1 10000000) 
     (rem  (* (f9s 1 10000) (f9s 10000 20000) (f9s 100000 200000) (f9s 1000000 2000000)) (expt 10 5)))
  (= 54176 ;;(f9s 1 100000000) 
     (rem  (* 79008 89312 96864 77152 48352) (expt 10 5))
     (f9s 1 800000))
  (= 38144 ;;(f9s 1 1000000000)
     (rem  (* 79008 89312 96864 77152 48352 46944) ;;(f9s 100000000 200000000) 
	   (expt 10 5)))
  ;(= (f9s 1 1000000000) (f9s 1 1000000) (f9s 1000000 8000000))
  (= 38144 (rem (* 12544 91776) (expt 10 5)))
  (= 38144 (rem (* 54176 46944) (expt 10 5)))
  ;(= (f9s 1 1000000000) (f9s 1 1000000) (f9s 10000000 80000000))
  (= 46112 ;;(f9s 1 10000000000)
     (rem (* 38144 50848) (expt 10 5))) ;; (f9s 1000000000 2000000000)
  (= (f9s 1 (expt 10 11)) 
     (rem (* 46112 (f9s (expt 10 9) (* 2 (expt 10 9)))) (expt 10 5)))
  (= (f9s 1 (expt 10 12))
   (rem (* 46112 (f9s (expt 10 11)) (f9s 1000000000 2000000000)) (expt 10 5))))
;; problem160=> (time (f9s 100000000 200000000))
;; "Elapsed time: 228767.66 msecs"
;; 46944
;; problem160=> (time (f9s 1000000000 2000000000))
;; "Elapsed time: 2548729.864 msecs"
;; 50848
;; problem160=> (time (f9s 500000001 800000000))
;; "Elapsed time: 775592.629 msecs"
;; 89408
;; problem160> (time (f9s 1000000000 8000000000))
;; "Elapsed time: 3.8096723603922E7 msecs"
;; 93504
;; (rem (* (f9s 1 1000000000) (f9s 1 1000000) (f9s 1000000000 8000000000)) (expt 10 5))
;; user> (rem (* 54176 46944 93504) (expt 10 5))
;; 16576  

(comment (map #(f9s (+ 1 (* % (expt 10 4))) (* (+ 1 %) (expt 10 4))) (range 1 10)))
;; 79008 (89312 18176 53664 90432 96512 58784 52192 71552 48096)
(comment (map #(f9s (+ 1 (* % (expt 10 5))) (* (+ 1 %) (expt 10 5))) (range 1 10)))
;; 62496 (96864 73184 54528 64992 94816 992 12448 51936 75104)
(comment (map #(f9s (+ 1 (* % (expt 10 6))) (* (+ 1 %) (expt 10 6))) (range 1 10)))
;; 12544 (77152 81472 48416 1344 27104 59456 43424 23584 30528)

(defn f10c [start end]
  (let [ex (expt 10 12)
	e5 (expt 10 5)
	e4 (expt 10 4)]
    (loop [current start so-far 1 catch {}]
      (cond 
	(zero? (rem so-far 10)) (recur current (quot so-far 10) catch)
	(< end current) (list (rem so-far e5) catch)
	:else
	(recur 
	 (+ 1 current) (* current (rem so-far ex))
	 (if (zero? (mod (- current 1) e4)) 
	   (let [item (rem (* current (rem so-far ex)) e5)
		 c (- current 1)]
	     (assoc catch item (conj (catch item) c)))
	   catch)
	 )))))

(comment
  (def px (f10c 1 10000000))
  ;;"Elapsed time: 22816.756363 msecs"
  )

(defn fval [px value]
  (for [k (keys (second px)) :when  
	(contains? (set ((second px) k)) value)] 
    (list k ((second px) k))))

;;;---------------------------

;;problem160> (time (f9 2560000))
;;"Elapsed time: 3747.83221 msecs"
;;16576

;; F(10^12) = F(2560000) 
;; F(2560000) = F(2550000) * PROD(2550001, 2560000) 
;; F(2550000) = F(500000) * PROD(500001, 510000) 
;; F(500000) = F(12500) * PROD(12501, 20000) 
;; F(12500) = easy enough 

(comment
 (time (rem (* (f9s 1 12500) 
	       (f9s 12501 20000) 
	       (f9s 500001 510000)
	       (f9s 2550001 2560000)) 
	    (expt 10 5))))
;;"Elapsed time: 77.159839 msecs"
;;16576

