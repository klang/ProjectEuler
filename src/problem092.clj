;; A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.

;; For example,

;; 44  32  13  10  1  1
;; 85  89  145  42  20  4  16  37  58  89

;; Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

;; How many starting numbers below ten million will arrive at 89?
(load "tools")
(use 'clojure.contrib.combinatorics 'clojure.set)

(defn square-and-add [number]
  (reduce + (map #(* % %) (digit-list number))))

(defn square-and-add [number]
  (let [nums [0 1 4 9 16 25 36 49 64 81]]
    (reduce + (map #(nums %) (digit-list number)))))

;; while inside the function, we accept mutation of the data
(defn number-chain [number]
  (loop [n number, c89 0, c1 0, chain (transient [])] 
    (if (or (< 1 c89) (< 1 c1)) 
      (persistent! chain)
      (recur (square-and-add n) 
	     (if (= n 89) (inc c89) c89)
	     (if (= n 1) (inc c1) c1)
	     (conj! chain n)))))

(defn number-chain-ending-in-89 [number] 
  (= 89 (last (number-chain number))))

(defn number-chain-ending-in-89 [number]
  (loop [n number] 
    (if (= n 89)  
      true
      (if (= n 1)
	false
	(recur (square-and-add n))))))

;; user> (time (count (filter #(number-chain-ending-in-89 %) (range 1 10000001))))
;; "Elapsed time: 542034.277862 msecs"
;; 8581146

;; ----- not a practical approach as it takes way too long

;; user> (time (count (range 1 10000001)))
;; "Elapsed time: 6656.455711 msecs"
;; 10000000

;; counting to 10 million without doing anything, takes a bit of time.

;; .. we have to reduce the number of calculations.

;; user> (= (number-chain-ending-in-89 12) (number-chain-ending-in-89 21) (number-chain-ending-in-89 210))
;; true

;; 12=21=210=201=102=120..=120000000, 10=01=1, 55=55, 155=551=515
(defn special-digit-list [number]
  (loop [n number dl ()]
    (if (zero? n) (sort dl)
      (if (zero? ( rem n 10)) 
	(recur (quot n 10) dl) 
	(recur (quot n 10) (conj dl (rem n 10)))))))
;; (count (digits number)) = n
;; n * quot + n * rem + 2 * n * zero?

;; sort by digits
;;  key (set)      value (vector)
;; {[4 1]          (number of digit that reduce to this key)
;;  [1]            (number of digit that reduce to this key)
;;  ...}

(defn digit-sorter [max]
  (let [sorted-digits (ref {})]
    (loop [ds (range 1 max)] ; 1 2 3 ... 9999 (4 digits)
      (if (= '() ds)
	sorted-digits
	(let [number (first ds)
	      sds  (special-digit-list number)]
	  (dosync (alter sorted-digits
			 #(assoc % sds
				 (if (% sds) (+ (% sds) 1) 1))))
	  (recur (rest ds)))))))

;; user> (time (def sorted-digits (digit-sorter 10000001)))
;; "Elapsed time: 371047.418636 msecs"

;; -- still too long time .. but let's use the result anyway

;;          1  2   3   4    5    6   7   8  count
;;       11 2 nil                               9
;;      101 3  2 nil                           54
;;     1001 4  6   6 nil                      219
;;    10001 5 12  24  24  nil                 714
;;   100001 6 20  60 120  120  nil           2001 
;;  1000001 7 30 120 360  720  720  nil     
;; 10000001 8 42 210 840 2520 5040 5040 nil 11439

(defn count-chains [sorted-digits]
  (loop [sd @sorted-digits
	 cc 0]
    (if (= '() sd) cc
	(let [item (first sd)
	      ending (number-chain-ending-in-89 (integer (first item)))
	      amount (second item)]
	  (recur (rest sd) (if ending (+ cc amount) cc))))))

;; (count-chains sorted-digits)
;; 8581146

;; -- after the fact optimizations (reading the forum for ideas)

(defn chains [n]
  ;; m [0 1 2 3 4 ... 44 .. 567]
  ;;    f f t t t      f      t
  (let [m (vec (cons false (map #(number-chain-ending-in-89 %) (range 1 568))))]
    (loop [total 0
	   number (range 1 n)]
      (if (= '() number)
	total
	(recur (if (m (square-and-add (first number))) (inc total) total) (rest number))))))

;; user> (time (chains 10000001))
;; "Elapsed time: 198245.863744 msecs"
;; 8581146

;; it still takes a lot of time
;; putting square-and-add inside chains gives this time:
;; "Elapsed time: 176651.667125 msecs"

