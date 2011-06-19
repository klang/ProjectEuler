(ns tools.pi
  (:use clojure.contrib.math)
  (:use clojure.test))

(def continued-fraction-e 
     (lazy-cat '(2) 
	       (interleave (cycle [1]) 
			   (map #(* 2 %) (iterate inc 1)) 
			   (cycle [1]))))

(defn estimate-continued-fraction [fraction]
  (+ (first fraction) 
     (if (= '() (rest fraction))
       0
       (/ (estimate-continued-fraction (rest fraction))))))

;; (estimate-continued-fraction (take 10 continued-fraction-e))

;;(/ (Math/pow -1 n) (inc (* 2 n))) 

(def pi (map #(/ (expt -1 %) (+ (* 2 %) 1)) (iterate inc 0)))
(def pi (map #(/ (* 4 (expt -1 %)) (+ (* 2 %) 1)) (iterate inc 0)))

;; 1
;; the nth element of the series
(defn pi-series [n] 
  (/ (* 4 (Math/pow -1 n)) (inc (* 2 n))))

;; lazy sequence giving the individual terms
(def pi1 (map #(pi-series %) (iterate inc 0)))
;; basically the same, but will use fractions (slower, but nice to look at)
(def pi2 (map #(/ (* 4 (expt -1 %)) (+ (* 2 %) 1)) (iterate inc 0)))

;; tools.pi> (time (double (reduce + (take 1000 pi1))))
;; "Elapsed time: 38.863614 msecs"
;; 3.140592653839794
;; tools.pi> (time (double (reduce + (take 1000 pi2))))
;; "Elapsed time: 5230.369837 msecs"
;; 3.140592653839793
;; (the fraction will be converted by double, but is inspectable)

;; (double (reduce + (take 650 pi)))
;; takes a while to converge, to 2 decimal points

;; this function is not tail recursive and as such will blow the stack for large values
(defn sums [i j]
  (cond (zero? i) (reduce + (map #(pi-series %) (range i (+ j 1))))
	:else
	(/ (+ (sums (- i 1) j) (sums (- i 1) (+ j 1))) 2)))

;; this gets the job done, but it's slow as sin.

;; let's see if we can do something about that

;; 1
(defn pi-formular [n] 
  (/ (* 4 (Math/pow -1 n)) (inc (* 2 n))))

(defn pi-formular-slow [n] 
  (/ (* 4 (expt -1 n)) (+ (* 2 n) 1)))

(defn series [f] (map #(f %) (iterate inc 0)))
;; (take 10 (series pi-formular-slow))
;; (4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15 4/17 -4/19)

;; 2
;; first elemet will be whatever the initial element for iterate is
(defn sums []
  (map first (iterate (fn [[a b]] [(+ (pi-formular b) a) (inc b)]) [4 1])))
(defn sums-slow []
  (map first (iterate (fn [[a b]] [(+ (pi-formular-slow b) a) (inc b)]) [4 1])))

;; by dropping the first element, we do not have to initialize in a special way
(defn sums-slow []
  (map first (drop 1 (iterate (fn [[a b]] [(+ (pi-formular-slow b) a) (inc b)]) [0 0]))))

;; by giving the series generator as an argument, we only need one function
(defn sums [f]
  (map first (drop 1 (iterate (fn [[a b]] [(+ (f b) a) (inc b)]) [0 0]))))
;(take 10 (sums pi-formular-slow))
;; it would be better if sums accepted a sequence, when we have to do average dampning
(defn sums-seq [sequence]
  (map first (drop 1 (iterate (fn [[a b]] [(+ (first b) a) (rest b)]) [0 sequence]))))
;;(take 10 (sums-seq (series pi-formular-slow)))

;; well, that seems to do the job
(deftest test-sums-seq
  ;;             (4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15 4/17 -4/19)     
  (is (= (list    4 
	       (+ 4 -4/3) 
	       (+ 4 -4/3 4/5) 
	       (+ 4 -4/3 4/5 -4/7) 
	       (+ 4 -4/3 4/5 -4/7 4/9) 
	       (+ 4 -4/3 4/5 -4/7 4/9 -4/11) 
	       (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13)
	       (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15)
	       (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15 4/17)
	       (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15 4/17 -4/19))
	 (take 10 (sums-seq (series pi-formular-slow)))
	 (take 10 (sums pi-formular-slow)))))

;; 3
(defn damping [seq]
  (map #(/ % 2) seq))

(defn damping [sequence]
  (map first (drop 1 (iterate (fn [[a b]] [(/ (+ (first b) (second b)) 2) (rest b)]) [0 sequence]))))

(deftest test-sums-seq
  (is (= (list (/ (+ 4 
		     (+ 4 -4/3)) 2) 
	       (/ (+ (+ 4 -4/3) 
		     (+ 4 -4/3 4/5)) 2) 
	       (/ (+ (+ 4 -4/3 4/5) 
		     (+ 4 -4/3 4/5 -4/7)) 2) 
	       (/ (+ (+ 4 -4/3 4/5 -4/7) 
		     (+ 4 -4/3 4/5 -4/7 4/9)) 2) 
	       (/ (+ (+ 4 -4/3 4/5 -4/7 4/9) 
		     (+ 4 -4/3 4/5 -4/7 4/9 -4/11)) 2) 
	       (/ (+ (+ 4 -4/3 4/5 -4/7 4/9 -4/11) 
		     (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13)) 2)
	       (/ (+ (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13) 
		     (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15)) 2)
	       (/ (+ (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15) 
		     (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15 4/17))2)
	       (/ (+ (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15 4/17) 
		     (+ 4 -4/3 4/5 -4/7 4/9 -4/11 4/13 -4/15 4/17 -4/19))2))
	 '(10/3 46/15 334/105 982/315 10942/3465 140986/45045 28382/9009 2400458/765765 45788882/14549535)
	 (take 9 (damping (sums-seq (series pi-formular-slow))))
	 '(10/3 46/15 334/105 982/315 10942/3465 140986/45045 28382/9009 2400458/765765 45788882/14549535)
	 (take 9 (damping (sums pi-formular-slow)))
	 '(10/3 46/15 334/105 982/315 10942/3465 140986/45045 28382/9009 2400458/765765 45788882/14549535))))

;; 4
(comment
  (double (first (damping (damping (damping (damping (damping (sums-seq (series pi-formular-slow)))))))))
  (double (first ((comp damping damping damping damping damping) (sums-seq (series pi-formular-slow)))))
  (double (first ((apply comp [damping damping damping damping damping]) (sums-seq (series pi-formular-slow))))))

(defn dampn [n seq]
  ((apply comp (repeat n damping)) seq))

(comment
  (double (first (dampn 5 (sums-seq (series pi-formular-slow))))))

(defn dampn [n]
  (apply comp (repeat n damping)))

(comment
  (double (first ((dampn 5) (sums-seq (series pi-formular-slow))))))

(defn sums [i j]
  (cond (zero? i) (reduce + (map #(pi-formular %) (range i (+ j 1))))
	:else
	(/ (+ (sums (- i 1) j) (sums (- i 1) (+ j 1))) 2)))



(def pi-continued-fraction [3 7 15 1 292 1 1 1 2 1 3 1 14 2 1 1 2 2 2 2 1 84 2 1 1 15 3 13 1 4 2 6 6 99 1 2 2 6 3 5 1 1 6 8 1 7 1 2 3 7 1 2 1 1 12 1 1 1 3 1 1 8 1 1 2 1 6 1 1 5 2 2 3 1 2 4 4 16 1 161 45 1 22 1 2 2 1 4 1 2 24 1 2 1 3 1 2 1])