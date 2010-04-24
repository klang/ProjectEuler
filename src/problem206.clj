(ns problem206
  (:use tools.numbers)
  (:use clojure.contrib.math)
  (:use clojure.contrib.combinatorics)
  (:use clojure.test))

;;Find the unique positive integer whose square has the form 1_2_3_4_5_6_7_8_9_0, where each “_” is a single digit.

;; we only have to look at squares in this 
(def total-range (range (first (exact-integer-sqrt 102030405060708090)) 
			(+ 1 (first (exact-integer-sqrt 192939495969798999)))))
;; (range 319421985 (+ 1 439248785))
;; (- (first (exact-integer-sqrt 192939495969798999)) (first (exact-integer-sqrt 102030405060708090)))
;; = 119826800 ==> the range is still too long

;; problem206> (time (count (filter #(not (nil? %)) (take 1000000 (map #(naivematch (* % %)) total-range)))))
;; "Elapsed time: 15765.396409 msecs"
;; 0

;; if we can redefine the range to 'step' in the right way
;; (integer (interleave [1 2 3 4 5 6 7 8 9] [0 0 0 0 0 0 0 0 1]))
;; counting in the last vector, will produce numbers on the correct form
;; -- but then we will have to do too many sqrt operations, which is quite expensive
;; (expt 10 9) combinations .. 

;; but there are only about a 10th of those combinations that can be produced by squaring a number in the range 
;; (- (first (exact-integer-sqrt 192939495969798999)) (first (exact-integer-sqrt 102030405060708090)))
;; 119826800

;; doing a simple subtraction, will show the number in the range
;; (- 102030405060738191 102030405060708090)
;; not very usefull information .. 

;; starting from (first (exact-integer-sqrt 1020304050607080900))
;; we have to find a number that when squared, has a 9 in the second position

(defn naivematch [n]
  "very very slow way t match the target value"
  (re-matches #"1.2.3.4.5.6.7.8.9." (.toString n)))

(defn d9? [number] (= 9 (quot (rem number 100) 10)))         ;; (expt 10 2)
(defn d8? [number] (= 8 (quot (rem number 10000) 1000)))     ;; (expt 10 4)
(defn d7? [number] (= 7 (quot (rem number 1000000) 100000))) ;; (expt 10 6)
(defn d6? [number] (= 6 (quot (rem number 100000000) 10000000))) ;; (expt 10 8)
(defn d5? [number] (= 5 (quot (rem number 10000000000) 1000000000))) ;; (expt 10 8)
(defn d4? [number] (= 4 (quot (rem number 1000000000000) 100000000000))) ;; (expt 10 8)
(defn d3? [number] (= 3 (quot (rem number 100000000000000) 10000000000000))) ;; (expt 10 8)
(defn d2? [number] (= 2 (quot (rem number 10000000000000000) 1000000000000000))) ;; (expt 10 8)
(defn d1? [number] (= 1 (quot (rem number 1000000000000000000) 1000000000000000))) ;; (expt 10 8)

(comment
  (time (count (filter #(and (d9? %) (d8? %) (d7? %) 
			     (d6? %) (d5? %) (d4? %) 
			     (d3? %) (d2? %) (d1? %)) (map #(* % %) (take 1000000 total-range))))))
;;"Elapsed time: 80182.100882 msecs"
;; 0
;;(filter #(and (d9? %) (d8? %) (d7? %) (d6? %) (d5? %) (d4? %) (d3? %)) (map #(* % %) (take 1000000 (drop 1000000 total-range))))
;; ()
;;(- 119826800 2000000)
;; .. then we only need to do that 1196 times more .. not practical

;; we need some higher jumps ..
;; to get 9 in the second position
;; (map sqrt (filter d9? (map #(* % %) (range 1 100))))
;; it seems the number has to end in one of 14 36 64 86 114 136 164 186 ..
;; we can jump by alternating 22 and 28 steps.., starting with 28 from the first number ending in 86 .. 
;; .. far out, but it reduces the range
;; problem206> (/ 119826800 (/ (+ 22 28) 2))
;; 4793072
;; to end in an 8 on the forth position..
;; (map sqrt (take-while #(< % (expt 10 8) ) (filter #(and (d8? %) (d9? %)) (map #(* % %) (range 1 4000)))))
;; (136 314 386 564 836 1014 1486 1664 1936 2114 2186 2364 2636 2814 2886 3064 3336 3514 3986)

(defn f []
  (loop [total-range (range (+ 1 (first (exact-integer-sqrt 102030405060708090))) 
			    (+ 1 (first (exact-integer-sqrt 192939495969798999))))
	 jump (cycle [28 22])
	 limit 1]
    ;; as we start from 319421986, we know that the second to last digit in
    ;; the square is 9, if the fourth to last digit is not 8, we jump
    (if (< limit 10000)
      (let [square (* (first total-range) (first total-range))]
	(if (d8? square) 
	  (if (d7? square)
	    (if (d6? square)
	      (if (d5? square)
		(if (d4? square)
		  (if (d3? square)
		    (if (d2? square)
		      square
		      (do (println (list :2 square limit))))
		    (do (println (list :3 square limit))))
		  (do (println (list :4 square limit))))
		(do (println (list :5 square limit))))
	      (do (println (list :6 square limit))))
	    (do (println (list :7 square limit))))
	  (do (println (list :8 square limit))))
	(recur (drop (first jump) total-range) (rest jump) (inc limit)))
      nil))
  )

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

;; (exact-integer-sqrt n) ==> [1_2_3_4_5_6_7_8_9_0 0]
;; number of digits in sqrt: 19
;; first digit:               1
;; last digit:                0