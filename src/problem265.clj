(ns problem265
  (:use 
   [clojure.math.numeric-tower :only (expt)]
   [clojure.test :only (deftest is)]))

;;00010111
;;000
;; 001
;;  010
;;   101
;;    011
;;     111
;;      110
;;       100
(comment
  (map #(apply str %) (take 8 (partition 3 1 (cycle "00010111")))))
;;00011101
(comment
  (map #(apply str %) (take 8 (partition 3 1 (cycle "00011101")))))

(comment
  (count (distinct (map #(apply str %) (take 8 (partition 3 1 (cycle "00011101")))))))

;; all possible N=3 (2³) strings
(defn binary-digits [N]
  (map #(. Integer toBinaryString %) (range (expt 2 (expt 2 N))
					    (inc (expt 2 (expt 2 N))))))

;; in the case 2^N with N=3, there are 2^2^3=256 different circular
;; arrangements, including all rotations, covering all subsequences.

;; As 000 has to be the first distinct number, then it is sufficient to
;; look at numbers starting with [000]10000.
;; This narrows the search volumen to (range (expt 2 4) (expt 2 5))

(defn binary-digits [N]
  (map #(. Integer toBinaryString %) (range (expt 2 (expt 2 N))
					    (inc (expt 2 (expt 2 N))))))

(defn binary-digits [N]
  (map #(. Integer toBinaryString %) (range (expt 2 (expt 2 N))
					    (inc (expt 2 (expt 2 N))))))


(defn number-of-digits [N]
  (- (inc (expt 2 (expt 2 N)))
     (expt 2 (expt 2 N))))

(comment
  (- (expt 2 (expt 2 3)) (expt 2 (- (- (expt 2 3) 3) 1)))
  (- (expt 2 (expt 2 5)) (expt 2 (- (- (expt 2 5) 5) 1))))

;;
(defn binary-counter [N]
  (map #(. Integer toBinaryString %) (range 0 (expt 2 N))))

(comment
  (map count-distinct (map str (cycle "00000101001101100100011111") )))

;; manual construction
;;

;; 20 00000101111101010011
;; 20 00000101111100110101


(defn show-combinations
  "shows all the possible l-bit values when S i rotated"
  [S l]
  (map #(apply str %) (take (count S) (partition l 1 (cycle S)))))

(defn count-distinct [S]
  (let [a (count (distinct (map #(apply str %) (take (count S) (partition 5 1 (cycle S))))))]
    {:distinct a :length (count S) :ok (= a (count S))}))

(comment
  (count-distinct "00000101001101100100011111010111")
  (map bin2int (show-combinations "00000101001101100100011111010111" 5))
  (bin2int "00000101001101100100011111010111")
  )

(defn bin2int
  "returns the decimal representation of a binary string"
  [binary-string]
  (reduce + (map #(* (- (int  %1) 48) (expt 2 %2)) 
		 (reverse binary-string) (iterate inc 0))))

(comment
  (map bin2int (map #(apply str %) (take 8 (partition 3 1 (cycle "00010111")))))
  ;;(0 1 2 5 3 7 6 4)
  (map bin2int (map #(apply str %) (take 8 (partition 3 1 (cycle "00011101")))))
  ;;(0 1 3 7 6 5 2 4)
  )



(deftest test-bin2int
  (is (= 234 (bin2int (. Integer toBinaryString 234)))))

(comment
  (bin2int "00000101000000000000000000000000")
  83886080
  (bin2int "00000101111100000000000000000000")
  99614720)

(comment
  (take 10 (filter #(= 16 (. Integer bitCount %)) (range 83886080 99614720)))
  (filter #(= 32 (:distinct %))
	  (map count-distinct
	       (map #(str "00000" (. Integer toBinaryString %))
		    (filter #(= 16 (. Integer bitCount %))
			    (range 83886080 99614720)))))

  (filter #(= 32 (:distinct %))
	  (map count-distinct
	       (map #(str "00000" (. Integer toBinaryString %))
		    (filter #(= 16 (. Integer bitCount %))
			    [87443415]))))
  )
(comment
  ;; sidenote:
  ;; the distance between integers that have 16 bits on
  ;; changes in a beautiful fashion
  (map #(- (second %) (first %))
       (partition 2 1
		  (take 1024
			(filter #(= 16 (. Integer bitCount %))
				(range 83886080 99614720)))))
  )

(comment
  "00000101001101100100011111010111"

  (bin2int (. Integer toBinaryString
	      (bit-and (bin2int "00000101001101100100011111010111")
		       (bin2int "00000000000000000000000000011111")))))

;; bit-and the mask and the candidate, and shift it right (range 27 0 -1) places
;; put the result into a hash, if it is already present in the hash, break and find
;;  another candidate
;; otherwise, try the next mask
;; if all masks produce different numbers, check the special masks

(deftest test-rotater
  (is (= (bin2int "00000101001101100100011111010111") 87443415))
  (is  (= (range 0 32)
	  (sort (map #(bit-and (. Integer rotateRight %1 %2) 31)
		     (repeat 87443415)
		     (range 0 32))))))

(defn count-distinct
 [i]
 (count (distinct
	 ;; consider the 5 least significant bits bits = 11111 = 31
	 (map #(bit-and (. Integer rotateRight %1 %2) 31) 
	      (repeat (int i)) (range 0 32)))))

(deftest test-count-distinct
  (is (= 32 (count-distinct 87443415))))


;; odd? == (not (= 0 (bit-and n 1)))

(defn find-rotations []
  (for [n (range 85539551 131913257) :when (and (= 16 (. Integer bitCount n)) (= 32 (count-distinct n)))] n))

(comment
  (defn find-rotations []
    (for [n (range 85539551 (+ 85539551 1000000)) :when (and (odd? n) (= 16 (. Integer bitCount n)) (= 32 (count-distinct n)))] n))

  (map #(. Integer toBinaryString %) l)

  (defn find-rotations []
    (for [n (range 131913257 (- 131913257 1000000) -1) :when (and (= 16 (. Integer bitCount n)) (= 32 (count-distinct n)))] n))

  (map #(. Integer toBinaryString %) m)
  )



;; The highest 5 bits have to be zero and the 27th bit has to be 1
;; (bin2int "00000100000000000000000000000000")  67108864
;; (bin2int "00000111111111111111111111111111") 134217727 (any number above and the representation clause is broken)
(comment
  (first (for [n (range 134217727 67108864 -1)
	       :when (and (odd? n)
			  (= 16 (. Integer bitCount n))
			  (= 32 (count-distinct n)))]
	   n))
  )
;; 131913257

;; using the lower bound
;; (bin2int "00000100000000000000000000000000") 67108864
(comment
  (first (for [n (range 67108864 83886080)
	       :when (and (odd? n)
			  (= 16 (. Integer bitCount n))
			  (= 32 (count-distinct n)))]
	   n))
  )

;; 73743071
;; problem265> (. Integer toBinaryString 73743071)
;; "100011001010011101011011111"

(defn run-through
  []
  (let [e (int 131913257)]	     ; 134217727 (+ 85539551 1000000) 
    (loop [n (int 73743071) sum (long 0)]
      (if (< e n)
	 sum
	(recur (+ n (int 2))
	       (if (and (= 16 (. Integer bitCount n))
			(= 32 (count-distinct n))) (+ sum (long n)) sum))))))

;;problem265> (time (run-through))
;;"Elapsed time: 631035.639787 msecs"
;;209110240768

;;;....

;; http://en.wikipedia.org/wiki/De_Bruijn_sequence
;; http://oeis.org/A065583
(use '[tools.numbers :only (factorial)] )

(defn B [k n]
  (quot (expt (factorial k) (expt k (- n 1))) (expt k n)))

;;(B 2 5)
(defn S [n]
  (quot (* (factorial n) (- (expt n (- n 1)) 1)) (* 2 (- n 1))))


(defn count-distinct
 [i]
 (count (distinct
	 ;; consider the 5 least significant bits bits = 11111 = 31
	 (map #(bit-and (. Integer rotateRight i %) 31) 
	      [0   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
	       16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31]))))

(defn count-distinct
 [i]
 (count (into #{}
	 ;; consider the 5 least significant bits bits = 11111 = 31
	 (map #(bit-and (. Integer rotateRight i %) 31) 
	      [0   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
	       16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31]))))
(defn count-distinct
 [i]
 (count (into #{}
	 ;; consider the 5 least significant bits bits = 11111 = 31
	 (map #(bit-and (. Integer rotateRight i %) 31) 
	      [0   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
	       16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31]))))


(defn run-through-2
  []
  (let [e (int (+ 1000000 73743071))] ;; 131913257
    (loop [n (int 73743071) sum (long 0)]
      (if (< e n)
	 sum
	(recur (+ n (int 2))
	       (if (and (= 16 (. Integer bitCount n))
			(= 32 (count-distinct n))) (+ sum (long n)) sum))))))


(comment
  ;; original form, running through 1 million elements
  "Elapsed time: 6880.382304 msecs"
  ;; stop using repeat in count-distinct
  "Elapsed time: 5789.087455 msecs"
  ;; hardcode the range
  "Elapsed time: 4412.765772 msecs"
  ;; using (into #{} .. instead of (distinct
  "Elapsed time: 1665.704695 msecs")

;; full run should take
;; (*  (/ 1665.704695 6880.382304 ) 631035.639787)
;; 152770.43941213164
;; problem265> (time (run-through))
;; "Elapsed time: 126994.129502 msecs"
;; 209110240768

;; defining the function as inline doesn't give anything
(comment
  (definline count-distinct
    "somethin"
    [i]
    `(count (into #{}
		  ;; consider the 5 least significant bits bits = 11111 = 31
		  (map #(bit-and (. Integer rotateRight ~i %) 31) 
		       [0   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
			16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31])))))

(defn problem265 [] (run-through))
