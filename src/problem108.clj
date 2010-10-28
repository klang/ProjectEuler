(ns problem108
  (meta
   {:description "In the following equation x, y, and n are positive integers.
1/x + 1/y = 1/n

For n = 4 there are exactly three distinct solutions:
1/5 + 1/20 = 1/6 + 1/12 = 1/8 + 1/8 = 1/4

What is the least value of n for which the number of distinct solutions exceeds one-thousand?

NOTE: This problem is an easier version of problem 110; it is strongly advised that you solve this one first." 
    })
  (:use [clojure.contrib.seq-utils :only (indexed)])
  (:use [clojure.contrib.math :only (gcd lcm)])
  (:use [tools.primes :only (factors)])
  (:use clojure.test))

(defn foo [b d] (/ (+ b d) (* b d)))
(defn bar [b d] (rem (* b d) (+ b d)))

;; there are solutions where x or y are negative too (ignored here)
;; analyzing the structure of the solutions by looking at wolfram aplha
;; http://www.wolframalpha.com/input/?i=(x%2By)+/+xy+%3D+1+/+4,+x%3E0,+y%3E0
(def manual {
	     1 #{[2 2]}
	     2 #{[3 6] [4 4]}
	     3 #{[4 12] [6 6]}
	     4 #{[5 20] [6 12] [8 8]}
	     5 #{[6 30] [10 10]}
	     6 #{[7 42] [8 24] [9 18] [10 15] [12 12]}
	     7 #{[8 56] [14 14]}
	     8 #{[9 72] [10 40] [12 24] [16 16]}
	     9 #{[10 90] [12 36] [18 18]}
	     10 #{[11 110] [12 60] [14 35] [15 30] [20 20]}
	     11 #{[12 132] [22 22]}
	     12 #{[13 156] [14 84] [15 60] [16 48] [18 36] [20 30] [21 28] [24 24]}
	     13 #{[14 182] [26 26]}
	     14 #{[15 210] [16 112] [18 63] [21 42] [28 28]}
	     15 #{[16 240] [18 90] [20 60] [24 40] [30 30]}
	     16 #{[17 272] [18 144] [20 80] [24 48] [32 32]} 
	     17 #{[18 306] [34 34]}
	     18 #{[19 342] [20 180] [21 126] [22 99] [24 72] [27 54] [30 45] [36 36]}
	     19 #{[20 380] [38 38]}
	     20 #{[21 420] [22 220] [25 100] [28 70] [30 60] [36 45] [40 40]}
	     21 #{[22 462] [24 168] [28 84] [30 70] [42 42]}
	     22 #{[23 506] [24 264] [26 143] [33 66] [44 44]}
	     23 #{[24 552] [46 46]}
	     24 #{[25 600] [26 312] [27 216] [28 168] [30 120] [32 96] [33 88] [36 72] [40 60] [42 56] [48 48]}
	     })

;; prime p : #{[p+1 p(p+1)] [2p 2p]}
;; nonprime: #{[n+1 n(n+1)] .. [2n 2n]}
;; 12 #{[13 156] [14 84] [15 60] [16 48] [18 36] [20 30] [21 28] [24 24]}

;;  #{[n+1 n(n+1)] [n+2 ] .. [2n 2n]}

;; (bit-shift-right (+ 156 12) 1) == (/ (+ 156 12) 2)

(defn ok?
  "returns true if target can be one of the fractions in 1/x + 1/y = 1/n"
  [n target]
  #_(= (/ 1 n) (+ (/ 1 (lcm target n)) (/ 1 target)))
  (let [y target x (lcm y n)] (= (* x y) (* n (+ x y)))))

;; there is probably a way to do this faster, buy simply checking if (gcd n target) = 1

(defn equation [n x y] (= (* x y) (* n (+ x y))))

(defn ok?
  "returns true if target can be one of the fractions in 1/x + 1/y = 1/n"
  [n target]
  (let [y target x (lcm y n)]
    (or (= (* x y) (* n (+ x y)))
	;; if gcd n y > 1 the equation might hold anyway, with this result
	(let [x2 (/ x (gcd n y))] (= (* x2 y) (* n (+ x2 y))))
	;; but this is not the only result there might be hidden somewhere
	)))


(defn ko? [n target] (< 1 (gcd n target)))

(defn solusions
  "[n+1 n(n+1)] and [2n 2n] are always solusions, avoid putting them through too many calculations"
  [n]
  (filter #(ok? n %) (range (+ 2 n) (* 2 n)))
  #_(filter #(ok? n %) (range (inc n) (inc (* 2 n)))))

(defn solusion-count [n]
  (+ 2 (count (solusions n)))
  #_(count (solusions n)))

(deftest test-solusions
  #_(is (= (map #(count (second %)) (sort manual))
	 (map solusions-count (range 1 25))))
  (is (= 3 (solusion-count 4)))
  (is (= 5 (solusion-count 6)))
  (is (= 4 (solusion-count 8)))
  (is (= 3 (solusion-count 9)))
  (is (= 5 (solusion-count 10)))
  (is (= 8 (solusion-count 12)))
  (is (= 5 (solusion-count 14)))
  (is (= 5 (solusion-count 15)))
  (is (= 5 (solusion-count 16)))
  (is (= 8 (solusion-count 18)))
  (is (= 7 (solusion-count 20)))
  (is (= 5 (solusion-count 21)))
  (is (= 5 (solusion-count 22)))
  (is (= 11 (solusion-count 24)))

  #_(is (every? true? (map #(= (second %) (solusion-count (first %)))
			 [[24 11] [78 14] [360 53] [1000 25] [1100 38] [6400 43] [15620 68] [150000 149]])))
  )

(defn debug [n]
  (map #(vector % (lcm % n) (gcd % n) (ok? n %)) (range (+ 2 n) (* 2 n))))
;; (ok n % (/ (lcm % n) (gcd % n)))

(defn reduce-while 
  "f is used on each item in coll until (pred (f(f(f item)))) fails"
  ([pred f coll]
     (let [s (seq coll)]
       (if  s
	 (reduce-while pred f (first s) (next s))
	 (f))))
  ([pred f val coll]
     (let [s (seq coll)]
       (if (and s (pred (f val (first s))))
	 (recur pred f (f val (first s)) (next s))
         val))))

(defn fraction [n y] (/ (+ y n) 2))
;; (bit-shift-right (+ 156 12) 1) == (/ (+ 156 12) 2)


;; problem108> (fraction 12 156)
;; 84
;; problem108> (fraction 12 84)
;; 48
;; problem108> (fraction 12 48)
;; 30
;; problem108> (fraction 12 30)
;; 21
;; problem108> (fraction 12 21)

;; prime p : #{[p+1 p(p+1)] [2p 2p]}
;; nonprime: #{[n+1 n(n+1)] .. [2n 2n]}
;; 12 #{[13 156] [14 84] [15 60] [16 48] [18 36] [20 30] [21 28] [24 24]}

;;  #{[n+1 n(n+1)] [n+2 ] .. [2n 2n]}

(defn x-is [n y] (/ (* n y) (- y n)))
(defn sols1 [n]
  (let [seed (* n (inc n))
	stop (* 2 n)
	x (inc n)
	first-known [x seed]
	last-known  [stop stop]]
    (loop [n n x x y seed catch []]
      (if (< y stop)
	(conj catch [stop stop])
	(recur n (inc x) (fraction n y) (conj catch [x y]))
	))))

(defn missing-is
  "retuns the missing value in 1/x + 1/y = 1/n"
  [n x] (/ (* n x) (- x n)))
(defn naive-solusions [n]
  (filter integer? (map #(missing-is n %) (range (inc n) (inc (* 2 n))))))
(defn naive-solusion-count [n] (count (naive-solusions n)))
;; problem108> (time (first (drop-while #(<= (naive-solusion-count %) 100) (iterate inc 1))))
;; "Elapsed time: 1215.892041 msecs"
;; 1260


(comment (take 200 (map #(vector (naive-solusion-count %) (factors %)) (iterate inc 1))))

(use 'clojure.contrib.pprint)
(comment
  (pprint (take 1260 (map #(vector % (naive-solusion-count %) (factors %)) (iterate inc 1))))
  (pprint (sort (take 1260 (map #(vector (naive-solusion-count %) % (factors %)) (iterate inc 1)))))
  (pprint (sort (take 1260 (map #(vector (factors %) (naive-solusion-count %) %) (iterate inc 1)))))
  (pprint (sort (take 1260 (map #(vector (count (distinct (factors %))) (naive-solusion-count %) (factors %) %) (iterate inc 1)))))
  ;; number of distinct factors of n has a relation to the number of solusions
  )

;; problem108> (naive-solusion-count (* 2 2 3 3 5 5 7 7 11))
;; 938
;; problem108> (* 2 2 3 3 5 5 7 7 11)
;; 485100
;; problem108> (naive-solusion-count (+ 1 (* 2 2 3 3 5 5 7 7 11)))
;; 2
;; problem108> (naive-solusion-count (* 2 2 2 3 3 5 5 7 7 11))
;; 1313
;; problem108> (* 2 2 2 3 3 5 5 7 7 11)
;; 970200 --> wrong
;; problem108>
;; (naive-solusion-count (* 2 2 2 2 3 3 3 3 5 7 11))
;; 1094
;; 498960 --> wrong
;; (naive-solusion-count (* 2 2 2 2 2 3 3 3 5 7 11))
;; 1040
;; 332640 (not tested)
;; (naive-solusion-count (* 2 2 2 2 7 3 3 5 7 11))
;; 1013
;; 388080 (larger than the above)


;; to see where jumps occur in a collection
(defn filter-smaller-than-seen
  "filters a collection so that only the first occurence of a max is returned."
  [coll]
  (loop [c coll
	 m (first c)
	 s [m]]
    (if (empty? c)
      s
      (if (>= m (first c))
	(recur (rest c) m s)
	(recur (rest c) (first c) (conj s (first c)))))))

(deftest test-filter-smaller-than-seen
  (is (=  (filter-smaller-than-seen [1 3 2 3 4]) [1 3 4]))
  (is (=  (filter-smaller-than-seen [1 2 3 4 5]) [1 2 3 4 5])))

;; the large jumps in number of solusions seem to happen at very certain points in the sequence.
(def first1260 (take 1260 (map #(vector % (naive-solusion-count %) (factors %)) (iterate inc 1))))
(defn filter-smaller-than-seen
  "filters a specific collection so that only the first occurence of a max is returned."
  [coll]
  (loop [c coll
	 m (first c)
	 s [m]]
    (if (empty? c)
      s
      (if (>= (second m) (second (first c)))
	(recur (rest c) m s)
	(recur (rest c) (first c) (conj s (first c)))))))

(comment
  (pprint (filter-smaller-than-seen (take 10000 (map #(vector % (naive-solusion-count %) (factors %)) (iterate inc 1)))))
  [[1       1 []]
   [2       2 [2]]
   [4       3 [2 2]]
   [6       5 [2 3]]
   [12      8 [2 2 3]]
   [24     11 [2 2 2 3]]
   [30     14 [2 3 5]]
   [60     23 [2 2 3 5]]
   [120    32 [2 2 2 3 5]]
   [180    38 [2 2 3 3 5]]
   [210    41 [2 3 5 7]]
   [360    53 [2 2 2 3 3 5]]
   [420    68 [2 2 3 5 7]]
   [840    95 [2 2 2 3 5 7]]
   [1260  113 [2 2 3 3 5 7]]
   [1680  122 [2 2 2 2 3 5 7]]
   [2520  158 [2 2 2 3 3 5 7]]
   [4620  203 [2 2 3 5 7 11]]
   [7560  221 [2 2 2 3 3 3 5 7]]
   [9240  284 [2 2 2 3 5 7 11]]
   [13860 338 [2 2 3 3 5 7 11]]
   ])

;; guessing that the next jump will happen at (* 2 2 2 2 3 5 7 11)
;; (naive-solusion-count (reduce * [2 2 2 2 3 5 7 11]))
;; 365

;; checking that the jump happened at the point we guessed at
(comment
  (pprint (filter-smaller-than-seen (take 6000 (map #(vector % (naive-solusion-count %) (factors %)) (iterate inc 13860)))))
  ;;[[13860 338 [2 2 3 3 5 7 11]] [18480 365 [2 2 2 2 3 5 7 11]]]
  )

;; the next two jumps will happen at the following points
;;problem108> (naive-solusion-count (reduce * [2 2 2 3 3 5 7 11]))
;;473
;;problem108> (naive-solusion-count (reduce * [2 2 3 3 5 7 11 13]))
;;1013
;, --> 180180

;; --- forum update
;; http://www.research.att.com/~njas/sequences/A018892

;; (2a1+1)(2a2+2)..(2an+1)+1>2000

;; k distinct primes in the factorisation of n, multiplying by any prime in that list
;; increases the number of solutions of the equation by 3^k.
;; (actually, 3^(k-1))

(deftest test-strage-observation
  (is (= 27 (- (naive-solusion-count (* 2 2 3 5 7)) (naive-solusion-count (* 2 3 5 7)))
	    (- (naive-solusion-count (* 2 3 3 5 7)) (naive-solusion-count (* 2 3 5 7)))
	    (- (naive-solusion-count (* 2 3 5 5 7)) (naive-solusion-count (* 2 3 5 7)))
	    (- (naive-solusion-count (* 2 3 5 7 7)) (naive-solusion-count (* 2 3 5 7)))))
  (is (= 81 (- (naive-solusion-count (* 2 2 3 5 7 11)) (naive-solusion-count (* 2 3 5 7 11)))
	    (- (naive-solusion-count (* 2 3 3 5 7 11)) (naive-solusion-count (* 2 3 5 7 11)))
	    (- (naive-solusion-count (* 2 3 5 5 7 11)) (naive-solusion-count (* 2 3 5 7 11)))
	    (- (naive-solusion-count (* 2 3 5 7 7 11)) (naive-solusion-count (* 2 3 5 7 11)))
	    (- (naive-solusion-count (* 2 3 5 7 11 11)) (naive-solusion-count (* 2 3 5 7 11)))))
  )
;; multiplying by two distinct primes from the list, increases the number of solusions of
;; the equation by 2^3 ? .. .. third by 7^2

;; python
;; def nsols(n):
;;     return sum(1 for x in range(n+1, n*2+1) 
;;                if ((n*x) % (x-n)==0))
;;  
;; f = 2*3*5*7*11*13
;; for i in xrange(f,f*17,f):
;;     if nsols(i)>=1000:
;;         print i
;;         break

(defn nsols [n]
  (count (filter #(zero? (rem (* n % ) (- % n))) (range (inc n) (inc (* 2 n))))))

(defn problem108 []
  (let [r (* 2 3 5 7 11 13)]
    (first (filter #(>= (nsols %) 1000) (range r (* r 17) r)))))

;; problem108> (time (problem108))
;; "Elapsed time: 4707.300194 msecs"
;; 180180

;; NOTICE: there is a sloane sequence that delivers filter-smaller-than-seen .. wow