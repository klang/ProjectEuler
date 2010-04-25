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
(defn dall? [n] (and (d0? n) (d9? n) (d8? n) (d7? n) (d6? n) (d5? n) (d4? n) (d3? n) (d2? n) (d1? n)))
(deftest test-d
  (is (d0? 1929394959697989990)) (is (d9? 1929394959697989990)) (is (d8? 1929394959697989990)) (is (d7? 1929394959697989990))
  (is (d6? 1929394959697989990)) (is (d5? 1929394959697989990)) (is (d4? 1929394959697989990)) (is (d3? 1929394959697989990))
  (is (d2? 1929394959697989990)) (is (d1? 1929394959697989990)) (is (dall? 1929394959697989990)))


(comment
  (time (count (filter #(and (d0? %) (d9? %) (d8? %) (d7? %) 
			     (d6? %) (d5? %) (d4? %) 
			     (d3? %) (d2? %) (d1? %)) (map #(* % %) (take 1000000 total-range))))))
;;"Elapsed time: 10751.909501 msecs"
;; 0
;;(filter #(and (d0? % ) (d9? %) (d8? %) (d7? %) (d6? %) (d5? %) (d4? %) (d3? %)) (map #(* % %) (take 1000000 (drop 1000000 total-range))))
;;problem206> (take 1 (map sqrt (filter #(and (d3? %) (d4? %) (d5? %) (d6? %) (d7? %) (d8? %) (d9? %)) (map #(* % %) total-range-10))))
;;(1022420830)

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

;; let's make a funcion instead of a one-liner .. (and use the correct range)
(defn jump-cycle [n]
  (loop [foo (take n (map sqrt (filter #(and (d8? %) (d9? %) (d0? %)) (map #(* % %) total-range)))) 
	 catch [(first foo)]]
    ;(println (first foo))
    (if (nil? (second foo))
      catch 
      (recur (rest foo) (conj catch (- (second foo) (first foo)))))))

;; A jump-cycle that matches 8_9_0 found by trial and error 
;;(jump-cycle 7)
;;[1010101670 100 300 840 300 100 860]
;; should return
;;{:start 1010101670 :cycle [100 300 840 300 100 860]}

(defn f []
  (let [end (+ 1 (first (exact-integer-sqrt 1929394959697989990)))
	start (first (exact-integer-sqrt 1020304050607080900))
	jc (jump-cycle 7)]
    (loop [current (first jc)
	   jump (cycle (rest jc))
	   limit 1]
      ;; as we start from 1010101030, we know that the third to last digit in
      ;; the square is 9 and the last is 0, if the fourth to last digit is not 8, we jump .. 
      (if (< current end) ; (< limit 1000000)		
	(let [square (* current current)]
	  (if (and ;(d0? square) (d9? square) (d8? square) 
	       (d7? square) (d6? square)  (d4? square) (d3? square)
	       (d5? square) (d2? square) (d1? square))
	    square
	    (recur (+ (first jump) current) (rest jump) (inc limit))))))))


;; problem206> (time (f))
;; "Elapsed time: 1819.530397 msecs"
;; 1929374254627488900
;; problem206> (dall? 1929374254627488900)
;; true
;; problem206> (exact-integer-sqrt 1929374254627488900)
;; [1389019170 0]

