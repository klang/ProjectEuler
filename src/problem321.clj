(ns problem321
  (meta {:description ""})
  (:use
   [clojure.test]
   [clojure.contrib.string :only (replace-re)]
   [clojure.contrib.math :only (expt round)]
   [clojure.contrib.generic.math-functions :only (sqrt)]
   [clojure.contrib.seq-utils :only (indexed)]
   [problem065 :only (estimate-continued-fraction)]))

;; R and B  moving towards the goal
(defn R_  [S] (replace-re #"R_" "_R" S))
(defn _B  [S] (replace-re #"_B" "B_" S))
(defn RB_ [S] (replace-re #"RB_" "_BR" S))
(defn _RB [S] (replace-re #"_RB" "BR_" S))

(deftest test-slide
  (is (= (R_ "RRR_BBB") "RR_RBBB"))
  (is (= (_B "RRR_BBB") "RRRB_BB")))

(deftest test-jump
  (is (= (RB_ "RRRB_BB") "RR_BRBB"))
  (is (= (_RB "RR_RBBB") "RRBR_BB")))

(deftest test-2 ;; (+ 1 2 3 2)
  (is (= "BB_RR"
	 (-> "RR_BB"
	     R_
	     _RB _B
	     RB_ RB_ _B
	     _RB R_))))

(deftest test-3 ; (+ 1 2 3 4 3 2)
  (is (= "_RBRBRB"
	 (-> "RRR_BBB"
	     R_
	     _RB _B
	     RB_ RB_ R_)))
  (is (= "BBB_RRR"
	 (-> "_RBRBRB"
	     _RB _RB _RB R_
	     RB_ RB_ _B
	     _RB R_))))

(deftest test-4 ; (+ 1 2 3 4 5 4 3 2)
  (is (= "RBRBRBRB_"
	 (-> "RRRR_BBBB"
	     R_
	     _RB _B
	     RB_ RB_ R_
	     _RB _RB _RB _B)))
  (is (= "BBBB_RRRR"
	 (-> "RBRBRBRB_"
	     RB_ RB_ RB_ RB_ _B
	     _RB _RB _RB R_
	     RB_ RB_ _B
	     _RB R_))))

(deftest test-5 ; (+ 1 2 3 4 5 6 5 4 3 2)
  (is (= "_RBRBRBRBRB"
	 (-> "RRRRR_BBBBB"
	     R_
	     _RB _B
	     RB_ RB_ R_
	     _RB _RB _RB _B
	     RB_ RB_ RB_ RB_ R_)))
  (is (= "BBBBB_RRRRR"
	 (-> "_RBRBRBRBRB"
	     _RB _RB _RB _RB _RB R_
	     RB_ RB_ RB_ RB_ _B
	     _RB _RB _RB R_
	     RB_ RB_ _B
	     _RB R_))))

(defn triangle [n] (quot (* n (+ n 1)) 2))

(defn triangles [] 
  (map (fn [n] (quot (* n (+ n 1)) 2)) (iterate inc 1)))
;; (take 10 (triangles)) --> (1 3 6 10 15 21 28 36 45 55)
;; M(2) != 10, as 10 is a triangle number and not in the 
;; list of the first 5 terms satisfying that [1 3 10 22 63]


;; (reduce + (range 0 (+ n 1)) === (* n (quot (- n 1) 2))
(defn M [n] (+ (triangle (+ n 1)) (- (triangle n) 1)))
(defn M [n] (+ (* n n) (* 2 n)))

(defn triangle?
  "implements the test given here:
  http://en.wikipedia.org/wiki/Triangular_number#Triangular_roots_and_tests_for_triangular_numbers"
  [n]
  (= (+ (* 8 (triangle n))1)
     (expt (+ (* 2 n) 1) 2)))

(defn triangle? [x]
  (integer? (+ -1 (sqrt (+ (* 8 x) 1)))))

(comment
  (filter triangle? (map M [1 3 10 22 63])))

(defn find-terms-under [n]
  (let [ts (into #{} (map triangle (range 1 n)))]
    (filter #(if (ts (second %)) % nil) (indexed (map M (range 0 n))))))

(comment
  (find-terms-under 1000000)
  ([1 3] [3 15] [10 120] [22 528] [63 4095] [133 17955] [372 139128] [780 609960] [2173 4726275] [4551 20720703] [12670 160554240] [26530 703893960] [73851 5454117903] [154633 23911673955] [430440 185279454480])

  (map first *1)
  (1 3 10 22 63 133 372 780 2173 4551 12670 26530 73851 154633 430440)

  (time (find-terms-under 2000000))
  ([1 3] [3 15] [10 120] [22 528] [63 4095] [133 17955] [372 139128] [780 609960] [2173 4726275] [4551 20720703] [12670 160554240] [26530 703893960] [73851 5454117903] [154633 23911673955] [430440 185279454480] [901272 812293020528])
  (map first *1)
  (1 3 10 22 63 133 372 780 2173 4551 12670 26530 73851 154633 430440 901272)
  )

;; brute forcing takes too long

;; http://www.wolframalpha.com/input/?i=n(n%2B1)/2+%3D+m%C2%B2+%2B2m

(defn f [k] (expt (- 3 (* 2 (sqrt 2))) k))
(defn g [k] (expt (+ 3 (* 2 (sqrt 2))) k))

(defn m1 [k]
  (round (/ (+ (- (* -4 (f k)) (* (sqrt 2) (f k)))
	       (- (+ (* 4 (g k)) (* (sqrt 2) (g k)))
		  8)) 8)))

(defn m2 [k] 
  (round (/ (+ (+ (* 4 (f k)) (* (sqrt 2) (f k)))
	       (- (- (* 4 (g k)) (* (sqrt 2) (g k)))
		  8)) 8)))

(defn n1 [k]
  (round
   (/ (+ (f k) (* -2 (sqrt 2) (f k)) (g k) (* 2 (sqrt 2) (g k)) -2) 4)))

(defn n2 [k]
  (round
   (/ (+ (- (f k)) (* 2 (sqrt 2) (f k)) (- (g k)) (* 2 (sqrt 2) (g k)) -2) 4)))



(def the-ms (take 40 (sort (into #{} (flatten (merge (map m1 (range 1 40)) (map m2 (range 1 40))))))))

;;(reduce + the-ms)
;;2470433131948039 -- not correct

(def the-ns (take 40 (sort (into #{} (flatten (merge (map n1 (range 1 40)) (map n2 (range 1 40))))))))
(defn sol-n [m] (/ (- (sqrt (+ (+ (* 8 (* m m)) (* 16 m))1))1)2))

;; the terms in the-ms run through M should equal the terms in the-ns run through triangle
(comment
  (map  #(if (= (M %1) (triangle %2)) (M %) {:m %1 :n %2 :diff (- %2 %1) :moves (M %1) :triangle (triangle %2) :sols-n (sol-n %1)}) the-ms the-ns)

  )
;; they do not, though .. sqrt 2 might not be as accurate as needed

;; it seems that sqrt is not precise enough when the numbers get very large
;; (the rounding error of (sqrt 2) is simply too large, luckily, we have a
;; function to estimate continued fractions to arbitrary precision, so with
;; a bit of changes to the formulars above, we should be able to produce a
;; better result
;; [1 2 2 2 2 2 ....]
(def continued-fraction-sqrt-2
     (lazy-cat '(1) (cycle [2])))

(def sqrt2 (estimate-continued-fraction (take 50 continued-fraction-sqrt-2)))
(defn fe [k] (expt (- 3 (* 2 sqrt2)) k))
(defn ge [k] (expt (+ 3 (* 2 sqrt2)) k))

(defn m1e [k]
  (round (/ (+ (- (* -4 (fe k)) (* sqrt2 (fe k))) (- (+ (* 4 (ge k)) (* sqrt2 (ge k))) 8)) 8)))
(defn m2e [k]
  (round (/ (+ (+ (* 4 (fe k)) (* sqrt2 (fe k))) (- (- (* 4 (ge k)) (* sqrt2 (ge k))) 8)) 8)))
(defn n1e [k]
  (round (/ (+ (fe k) (* -2 sqrt2 (fe k)) (ge k) (* 2 sqrt2 (ge k)) -2) 4)))
(defn n2e [k]
  (round (/ (+ (- (fe k)) (* 2 sqrt2 (fe k)) (- (ge k)) (* 2 sqrt2 (ge k)) -2) 4)))


(def the-mes (take 40 (sort (into #{} (flatten (merge (map m1e (range 1 40)) (map m2e (range 1 40))))))))
(def the-nes (take 40 (sort (into #{} (flatten (merge (map n1e (range 1 40)) (map n2e (range 1 40))))))))

(def check
     (map  #(if (= (M %1) (triangle %2)) (M %) {:m %1 :n %2 :diff (- %2 %1) :moves (M %1) :triangle (triangle %2) :sols-n (sol-n %1)}) the-mes the-nes))


;; (reduce + the-mes )
;; 2470433131948040
;;(- (reduce + the-mes) (reduce + the-ms))
;;1
;; so, using the build in sqrt puts the end result off by one!
