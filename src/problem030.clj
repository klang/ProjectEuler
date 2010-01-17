;; n! means n  (n  1)  ...  3  2  1

;; Find the sum of the digits in the number 100!

(use 'clojure.contrib.test-is
     '[clojure.contrib.math :only (expt)] 
     '[clojure.contrib.str-utils2 :only (split)])

(defn digits [n]
  (map #(. Integer parseInt % 10) 
	     (filter #(not (= % "")) (split (str n) #""))))

(defn sum-of-powers [pow number]
  (reduce + (map #(expt % pow) (digits number))))

(defn sum-of-powers? [pow number]
  (= number (sum-of-powers pow number)))

(filter #(sum-of-powers? 4 %) (range 2 10000))
(filter #(sum-of-powers? 5 %) (range 2 1000000))
;; (4150 4151 54748 92727 93084 194979)
; (filter #(sum-of-powers? 5 %) (range 2 1000000))
; (4150 4151 54748 92727 93084 194979)
; (filter #(sum-of-powers? 5 %) (range 2 100000000))
;; stopped .. takes too long
;; (filter #(sum-of-powers? 5 %) (range 1000000 2000000))
;; ()

(defn problem030 [n] 
  (reduce + 
	(filter #(sum-of-powers? n %) (range 2 1000000))))

(deftest test-problem030
  (is (sum-of-powers? 4 1634))
  (is (sum-of-powers? 4 8208))
  (is (sum-of-powers? 4 9474))
  (is (= 19316 (problem030 4)))
  )

; user> (time (problem030 5))
;; simply expect there to be no other candidates below 1000000 .. cheating for sure
;; (reduce + '(4150 4151 54748 92727 93084 194979))
;; 443839
; (run-tests)
