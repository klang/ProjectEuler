(ns tools.numbers
  (:use clojure.test)
  (:use [clojure.contrib.str-utils2 :only (split)])
  ;(:use [clojure.contrib.math :only (expt sqrt exact-integer-sqrt)])
  ;(:use [ clojure.contrib.lazy-seqs :only (primes)])
  (:use clojure.contrib.math)
  (:use clojure.contrib.repl-utils)
  (:use clojure.contrib.combinatorics)
  (:use clojure.set))


(defn digits [number]
  (map #(. Integer parseInt % 10) 
	     (filter #(not (= % "")) (split (str number) #""))))

(defn reverse-number [number]
  (loop [n number dl 0]
    (if (zero? n) dl
      (recur (quot n 10) (+ (* 10 dl) (rem n 10))))))

(defn palindrome? [n] (= (reverse-number n) n))

(defn digit-list [number]
  "convert number to digit list"
  (cond (zero? number) (list 0)
	:else
	(loop [n number dl ()]
	  (if (zero? n) dl
	      (recur (quot n 10) (conj dl (rem n 10)))))))

(defn digits [number] (digit-list number))

(defn digit-set [number]
  (loop [n number dl #{}]
    (if (zero? n) dl
      (recur (quot n 10) (conj dl (rem n 10))))))

(defn digit-list2number [digit-list]
  (reduce + (map #(* %1 (expt 10 %2)) 
		 (reverse digit-list) 
		 (range 0 (count digit-list)))))

(defn integer [digit-list]
  (digit-list2number digit-list))

(defmulti digits-odd? class)
(defmethod digits-odd? clojure.lang.LazySeq [list] 
  (every? odd? list))
(defmethod digits-odd? java.lang.Integer [number] 
  (every? odd? (digits number)))


(defn factorial [n] 
  (reduce * (range n 0 -1)))

(defn factorials []
  (map first (iterate (fn [[a b]] [(* a b) (inc b)]) [1 2])))

(defn fibos []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0 1])))

