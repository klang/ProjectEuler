;; What is the first term in the Fibonacci sequence to contain 1000 digits?

(use 'clojure.contrib.test-is
     '[clojure.contrib.str-utils2 :only (split)])

(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

(defn digits [n]
  (map #(. Integer parseInt % 10) 
	     (filter #(not (= % "")) (split (str n) #""))))

(defn fibo-terms []
  (iterate (fn [[c a b]] [(inc c) b (+ a b)]) [1 0 1]))

(defn problem025 [d]
  (first (take 1 (filter #(not (zero? %)) 
			 (map #(if (<= d (count (digits (nth % 2)))) 
				 (first %) 
				 0) 
			      (fibo-terms))))))

;; user> (time (problem025 1000))
;; "Elapsed time: 7902.703557 msecs"
;; 4782

(deftest test-problem025
  (is (= 12 (problem025 3))))

; (run-tests)
