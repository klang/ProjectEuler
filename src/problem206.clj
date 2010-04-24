(ns problem206
  (:use tools.numbers)
  (:use clojure.contrib.math)
  (:use clojure.contrib.combinatorics)
  (:use clojure.test))

;;Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0, where each “_” is a single digit.

;; we only have to look at squares in this 
(def total-range (range (first (exact-integer-sqrt 1020304050607080900)) 
			(+ 1 (first (exact-integer-sqrt 1929394959697989990)))))
;; (range 1010101010 (+ 1 1389026623))
;; (- (first (exact-integer-sqrt 1929394959697989990)) (first (exact-integer-sqrt 1020304050607080900)))
;; = 378925613 ==> the range is still too long

;; problem206> (time (count (filter #(not (nil? %)) (take 1000000 (map #(naivematch (* % %)) total-range)))))
;; if we can redefine the range to 'step' in the right way
;; (integer (interleave [1 2 3 4 5 6 7 8 9 0] [0 0 0 0 0 0 0 0 0 1]))
;; counting in the last vector, will produce numbers on the correct form
;; -- but then we will have to do too many sqrt operations, which is quite expensive
;; (expt 10 10) combinations .. 

;; but there are only about a 26th of those combinations that can be produced by squaring a number in the range 
;; (- (first (exact-integer-sqrt 1929394959697989990)) (first (exact-integer-sqrt 1020304050607080900)))
;; 378925613

;; doing a simple subtraction, will show the number in the range
;; (- 1020304050607381910 1020304050607080900)
;; not very usefull information .. 

;; starting from (first (exact-integer-sqrt 1020304050607080900))
;; we have to find a number that when squared, has a 9 in the third position

(defn naivematch [n]
  "very very slow way t match the target value"
  (re-matches #"1.2.3.4.5.6.7.8.9.0" (.toString n)))

(defn d0? [number] (= 0 (rem number 10))) ;; (expt 10 2)
(defn d9? [number] (= 9 (quot (rem number 1000)                 100)))         ;; (expt 10 2)
(defn d8? [number] (= 8 (quot (rem number 100000)               10000)))     ;; (expt 10 4)
(defn d7? [number] (= 7 (quot (rem number 10000000)             1000000))) ;; (expt 10 6)
(defn d6? [number] (= 6 (quot (rem number 1000000000)           100000000))) ;; (expt 10 8)
(defn d5? [number] (= 5 (quot (rem number 100000000000)         10000000000))) ;; (expt 10 8)
(defn d4? [number] (= 4 (quot (rem number 10000000000000)       1000000000000))) ;; (expt 10 8)
(defn d3? [number] (= 3 (quot (rem number 1000000000000000)     100000000000000))) ;; (expt 10 8)
(defn d2? [number] (= 2 (quot (rem number 100000000000000000)   10000000000000000))) ;; (expt 10 8)
(defn d1? [number] (= 1 (quot (rem number 10000000000000000000) 1000000000000000000))) ;; (expt 10 8)

(comment
  (time (count (filter #(and (d0? %) (d9? %) (d8? %) (d7? %) 
			     (d6? %) (d5? %) (d4? %) 
			     (d3? %) (d2? %) (d1? %)) (map #(* % %) (take 1000000 total-range))))))
;;"Elapsed time: 10751.909501 msecs"
;; 0
;;(filter #(and (d0? % ) (d9? %) (d8? %) (d7? %) (d6? %) (d5? %) (d4? %) (d3? %)) (map #(* % %) (take 1000000 (drop 1000000 total-range))))

;; we need some higher jumps ..
;; to get 9 in the third position AND 0 in the first position
;; (filter #(zero? (mod % 10)) (map sqrt (filter d9? (map #(* % %) (range 1 1000)))))
;; it seems the number has to end in one of 
;;(30 70 130 170 230 270 330 370 430 470 530 570 630 670 730 770 830 870 930 970)
;; we can jump by alternating 40 and 60 steps.., starting with 70 from the first number ending in 30 .. 
;; .. far out, but it reduces the range
;; problem206> (/ 378925613 (/ (+ 40 60) 2))
;; 7578513
;; to end in an 8 on the fifth position..
;; (map sqrt (take-while #(< % (expt 10 8) ) (filter #(and (d8? %) (d9? %)) (map #(* % %) (range 1 4000)))))
;; (430 530 830 1670 1970 2070 2930 3030 3330)

;; to calculate the jump cycle
(comment
  (loop [foo (map sqrt (filter #(and (d8? % ) (d9? %) (d0? %)) (map #(* % %) (range 1 10000)))) catch []] (if (= 10 (count foo)) catch (recur (rest foo) (conj catch (- (second foo) (first foo)))))))

;; let's make a funcion instead of a one-liner .. 

(defn jump-cycle [n]
  (loop [foo (map sqrt (filter #(and (d8? %) (d9? %) (d0? %)) (map #(* % %) (range 1 n)))) 
	 catch []]
    (println (first foo))
    (if (= 1 (count foo))
      catch 
      (recur (rest foo) (conj catch (- (second foo) (first foo)))))))

;; A jump-cycle that matches 8_9_0 found by trial and error 
;;(jump-cycle 3000)
;;[100 300 840 300 100 860]

(defn f []
  (let [end (+ 1 (first (exact-integer-sqrt 1929394959697989990)))
	start  (+ 20 (first (exact-integer-sqrt 1020304050607080900)))]
    (loop [current (+ 20 (first (exact-integer-sqrt 1020304050607080900)))
	   jump (cycle [1])
	   limit 1]
      ;; as we start from 1010101030, we know that the third to last digit in
      ;; the square is 9 and the last is 0, if the fourth to last digit is not 8, we jump .. 
      (if (< limit 1000) ; (< current end)
	(let [square (* current current)]
	  (if (d0? square)
	    (if (d9? square)
	      (if (d8? square)
		(if (d7? square)
		  (if (d6? square)
		    (if (d5? square)
		      (if (d4? square)
			(if (d3? square)
			  (if (d2? square)
			    square
			    (do (println (list :2 square current limit))
				(recur (+ (- 10 (mod current 10)) current) jump (inc limit))))
			  (do (println (list :3 square current limit))
			      (recur (+ (- 10 (mod current 10)) current) jump (inc limit))))
			(do (println (list :4 square current limit))
			    (recur (+ (- 10 (mod current 10)) current) jump (inc limit))))
		      (do (println (list :5 square current limit))
			  (recur (+ (- 10 (mod current 10)) current) jump (inc limit))))
		    (do (println (list :6 square current limit))
			(recur (+ (- 10 (mod current 10)) current) jump (inc limit))))
		  (do (println (list :7 square current limit))
		      (recur (+ (- 10 (mod current 10)) current) jump (inc limit))))
		(do (println (list :8 square current limit))
		    (recur (+ (- 10 (mod current 10)) current) jump (inc limit))))
	      (do (println (list :9 square current limit))
		  (recur (+ 10 current) jump (inc limit))))
	    (do ;(println (list :0 square current limit))
		(recur (+ (- 10 (mod current 10)) current) jump (inc limit))
		)))
	(list current (- current start)  (expt current 2))))))

(comment
(if (and (d8? square) (d7? square) (d6? square) (d5? square) 
		  (d4? square) (d3? square) (d2? square) (d1? square)))
)
(defn dd? [digit number] (= digit (quot (mod number (expt 10 (- 9 (- digit 3)))) (expt 10 (- 9 (- digit 1))))))
;;[2 4 6 8 10 12 14 15 16]
;;[9 8 7 6  5  4  3  2  1]


(defn d89? [n] (let [d9 (mod n 100)]))

;; 100 
;192939495969798999
;100000000000000000
;;; ---
; [1 2 3 4 5 6 7 8 9 0]
;  

(def s (map #(integer 
	 (drop-last 
	  (interleave [1 2 3 4 5 6 7 8 9 0] (cycle [%])))) 
       (range 0 10)))

(map #(exact-integer-sqrt %) s)

;([1010101010 204060800] [1058921220 1450893510] [1105587740 1788975320] [1150362705 582466905] [1193459029 745862099] [1235052450 1406583450] [1275290028 1151446176] [1314296297 1369875761] [1352177820 1787936580] [1389026623 295205861])

;; (exact-integer-sqrt n) ==> [1_2_3_4_5_6_7_8_9_0]
;; number of digits in sqrt: 19
;; first digit:               1
;; last digit:                0 --> jump by 10
;; second to last digit       9 --> jump by 10 * 3 or 10 * 7