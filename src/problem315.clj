(ns problem315
  (meta {:notation "http://en.wikipedia.org/wiki/Seven-segment_display"})
  (:use [clojure.test]
	[clojure.contrib.math :only (expt abs)]
	[tools.numbers :only (digits)]
	[tools.primes :only (primes-up-to)]))

;; seven segment digital clock notation
;;
;;    **A** 
;;   *     *
;;   F     B
;;   *     *
;;    **G**
;;   *     *
;;   E     C
;;   *     *
;;    **D**

;; Digit	gfedcba	abcdefg	a	b	c	d	e	f	g
;; 0		0x3F	0x7E	on	on	on	on	on	on	off
;; 1		0x06	0x30	off	on	on	off	off	off	off
;; 2		0x5B	0x6D	on	on	off	on	on	off	on
;; 3		0x4F	0x79	on	on	on	on	off	off	on
;; 4		0x66	0x33	off	on	on	off	off	on	on
;; 5		0x6D	0x5B	on	off	on	on	off	on	on
;; 6		0x7D	0x5F	on	off	on	on	on	on	on
;; 7		0x07	0x70	on	on	on	off	off	off	off
;; 7alt		0x27	0x72	on	on	on	off	off	on	off
;; 8		0x7F	0x7F	on	on	on	on	on	on	on
;; 9		0x6F	0x7B	on	on	on	on	off	on	on

(defn binary-string [n] (. Integer toBinaryString n))
(defn common-bits [d1 d2]
  (let [d1 (segments d1) d2 (segments d2)]
    (bit-count (bit-and d1 d2))))


(def segments
     (assoc (zipmap (range) [0x7E 0x30 0x6D 0x79 0x33 0x5B 0x5F 0x72 0x7F 0x7B])
       nil 0x00))

(defn bit-count [n] (. Integer bitCount n))
 
(defn bit-transition [d1 d2]
  (bit-count (bit-xor (segments d1) (segments d2))))

(deftest test-sam-max
  (is (= 40
	 (+
	  (* 2 (+ (bit-transition nil 1) (bit-transition nil 3) (bit-transition nil 7)))
	  (* 2 (+ (bit-transition nil 1) (bit-transition nil 1)))
	  (* 2 (bit-transition nil 2))))
      (= (* 2 (reduce + (map #(bit-count (segments %)) [1 3 7 1 1 2])))))
  
  (is (= 30 ;; 137->11->2
	 (+ (bit-transition nil 1) (bit-transition nil 3) (bit-transition nil 7)
	    (bit-transition 1 nil) (bit-transition 3 1) (bit-transition 7 1)
	    (bit-transition nil nil) (bit-transition 1 nil) (bit-transition 1 2)
	    (bit-transition 2 nil)))))

(deftest test-bit-transition
  (is (= (map (partial bit-transition nil)
	      [nil 0 1 2 3 4 5 6 7 8 9])
	 [0 6 2 5 5 4 5 6 4 7 6]))
  (is (= (map (partial bit-transition 8)
	      [nil 0 1 2 3 4 5 6 7 8 9])
	 [7   1 5 2 2 3 2 1 3 0 1]))
  (is (= (map (partial bit-transition 1)
	      [nil 0 1 2 3 4 5 6 7 8 9])
	        [2 4 0 5 3 2 5 6 2 5 4])))

(def primes (drop-while #(< % (expt 10 7)) (primes-up-to (* 2 (expt 10 7)))))
(defn digit-sum [n] (reduce + (digits n)))

;; 137 -> 11 -> 2

(defn digital-root [n]
  (loop [from (digits n)
	 to (digits (reduce + from))
	 total [from]]
    (if (= 1 (count to))
      (conj total to)
      (recur to (digits (reduce + to)) (conj total to)))))

(comment 
  (def f (map #(list (first %) (pad-coll (first %) (second %))) (partition 2 1 ['(nil)] (digital-root 137))))
  (map #(bit-transition %1 %2) '(1 3 7) '(nil 1 1))
  (map #(bit-transition %1 %2) '(1 1) '(nil 2))
  (map #(bit-transition %1 %2) '(2) '(nil))
  
  (map #(list (first %) (second %)) f)

  (+
   (reduce + (map #(bit-count (segments %)) (first (first f))))
   (reduce + (flatten (map #(transition (first %) (second %)) f))))
  ;; missing "from turned off to first value"
  )

(defn transition [coll1 coll2] (map #(bit-transition %1 %2) coll1 coll2))


(defn padding
  "makes two collections match in length by padding the shortest with nil entries"
  [coll1 coll2]
  (let [p (- (count coll1) (count coll2))]
    (if (neg? p)
      [(concat (take (abs p) (cycle [nil])) coll1) coll2]
      [coll1 (concat (take p (cycle [nil])) coll2) ])))

(deftest test-padding
  (is (= (padding '(1 3 7) '(1 1))   ['(1 3 7) '(nil 1 1)]))
  (is (= (padding '(1 1) '(1 3 7) )  ['(nil 1 1) '(1 3 7) ])))



(defn pad-coll
  "returns a padded version of coll"
  [coll1 & coll]
  (concat (take (- (count coll1) (count (first coll))) (cycle [nil])) (first coll)))


(defn t-max [n]
  (let [f (map #(list (first %) (pad-coll (first %) (second %)))
	       (partition 2 1 ['(nil)] (digital-root n)))]
    (+
     (reduce + (map #(bit-count (segments %)) (first (first f))))
     (reduce + (flatten (map #(transition (first %) (second %)) f))))))

;; problem315> (time (reduce + (map t-max primes)))
;; "Elapsed time: 110014.400207 msecs"
;; 49799480

(defn t-sam [n]
  (* 2 (reduce + (map #(bit-count (segments %)) (flatten (digital-root n))))))

;; problem315> (time (reduce + (map t-sam primes)))
;; "Elapsed time: 61036.646611 msecs"
;; 63424722

;; problem315> (- *1 *2)
;; 13625242

;; though this is the correct answer, the solution is not fast enough.

