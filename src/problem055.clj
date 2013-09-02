(ns problem055)
;; If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
;; 
;; Not all numbers produce palindromes so quickly. For example,
;; 
;; 349 + 943 = 1292,
;; 1292 + 2921 = 4213
;; 4213 + 3124 = 7337
;; 
;; That is, 349 took three iterations to arrive at a palindrome.
;; 
;; Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome. A number that never forms a palindrome through the reverse and add process is called a Lychrel number. Due to the theoretical nature of these numbers, and for the purpose of this problem, we shall assume that a number is Lychrel until proven otherwise. In addition you are given that for every number below ten-thousand, it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one, with all the computing power that exists, has managed so far to map it to a palindrome. In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome: 4668731596684224866951378664 (53 iterations, 28-digits).
;; 
;; Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
;; 
;; How many Lychrel numbers are there below ten-thousand?
;; 
;; NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.

(defn reverse-number [number]
  (loop [n number dl 0]
    (if (zero? n) dl
      (recur (quot n 10) (+ (* 10 dl) (rem n 10))))))

(defn palindrome? [n] 
  (= (reverse-number n) n))

(def lychrel-limit 50)

(defn lychrel-number? [number]
  (loop [i lychrel-limit 
	 n (+ (bigint number) (reverse-number number))]
    (if (palindrome? n)
      false ; (list number n i false)
      (if (zero? i)
	true ; (list number n i true)
	(recur (dec i) (+ n (reverse-number n)))))))

;; user> (lychrel-number? 196)
;; true
;; user> (lychrel-number? 47)
;; false
;; user> (lychrel-number? 4994)
;; true

(defn lychrel-numbers-under [limit]
  (count (filter #(lychrel-number? %) (range 1 (+ limit 1)))))

;; user> (time (lychrel-numbers-under 10000))
;; "Elapsed time: 869.360469 msecs"
;; 249

(defn problem055 [] (lychrel-numbers-under 10000))
