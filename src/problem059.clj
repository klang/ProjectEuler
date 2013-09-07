(ns problem059
  (:use 
   [clojure.test :only (deftest is)])
  (:require
   [clojure.string :only (split replace) :as str]
   [clojure.math.combinatorics :only (selections) :as comb]))


(defn word-value [w]  
  (reduce + (map #(- (int %) 64) w)))

(def cipher1-txt (slurp "src/cipher1.txt"))

(def cipher1 
     (map #(. Integer parseInt % 10) 
	  (map #(str/replace % "\r\n" "")	;; remove the 2 last chars in the file
	       (str/split cipher1-txt #","))))

;; user> (bit-xor 65 42)
;; 107
;; user> (bit-xor 107 42)
;; 65
;; (char 65)
;; \A
;; (int \A)
;; 65

(def uc (range (int \A) (+ 1 (int \Z))))
(def lc (range (int \a) (+ 1 (int \z))))
(def pool (for [p (range (int \A) (+ 1 (int \z))) 
		:when (not (>= (int \a) p (int \Z)))] p))
(def pool (range (int \a) (+ 1 (int \z))))
;; password is 3 lowercase letters
;;user> (count (selections pool 3))
;;17576

(def passwords (comb/selections pool 3))

; (apply str (map #(char (bit-xor %1 %2)) cipher1 (cycle (first passwords))))

(defn string-to-ascii-list [text]  (map #(int %) text))
(deftest test-string-to-ascii-list 
  (is (= '(97 98 99 100 101 102 103) (string-to-ascii-list "abcdefg"))))

(defn xor-text-list [text pass]
  (let [ascii-text (string-to-ascii-list text)
	ascii-pass (string-to-ascii-list pass)]
   (map #(bit-xor %1 %2) ascii-text (cycle ascii-pass))))
(deftest test-xor-text-list
  (let [text "the quick brown fox jumped over the lazy dog"
	pass "abc"]
    (is (= (string-to-ascii-list text) (xor-text-list (xor-text-list text pass) pass)))))

(defn xor-text-string [text pass]
  (apply str (map #(char %) (xor-text-list text pass))))
(deftest test-xor-text-string
  (let [text "the quick brown fox jumped over the lazy dog"
	pass "abc"]
    (is (= text (xor-text-string (xor-text-string text pass) pass)))
    (is (= nil (re-matches #"[a-zA-Z., ]+" (xor-text-string (xor-text-string text pass) "adf"))))
    (is (= text (re-matches #"[a-zA-Z., ]+" (xor-text-string (xor-text-string text pass) pass))))))

(def words-txt (slurp "src/words.txt"))
(def words (map #(str/replace % "\"" "") (str/split words-txt #",")))


(defn probably-char [c]
  (re-matches #"[a-zA-Z., ]" (str c)))

(defn decipher [text pass]
  (letfn [(d [password limit] 
	     (apply str 
		    (take limit 
			  (map #(char (bit-xor %1 %2)) 
			       text 
			       (cycle password)))))]
    (loop [password pass]
      (if (empty? password)
	nil                ;; no sensible decryption found with given password 
	(let [current (first password)
	      clear (apply str (map char current))
	      decrypted (d current 100)        
	      chars (filter #(re-matches #"[0-9a-zA-Z;:., ?()'!-]" (str %)) decrypted)
	      probability (/ (count chars) (count decrypted))]
	  ;; current password has a 99% probability of being correct
	  (if (< 99/100 probability)
	    (list clear (d current (count text)))
	    (recur (rest password))))
	))))

(deftest test-decipher
  (let [text "the quick brown fox jumped over the lazy dog"
	pass "abc"
	lpass (list (string-to-ascii-list pass))
	cipher (xor-text-list text pass)]
    (is (= (list pass text) (decipher cipher lpass)))
    (is (= (list pass text) (decipher cipher (list (string-to-ascii-list "abc")))))
    ))

;; (time (decipher cipher1 passwords))
;; "Elapsed time: 2472.662205 msecs"
;; ("god" "(The Gospel of John, chapter 1) ...")

;; if limit is (count text) instead of just 100, the execution takes
;; "Elapsed time: 29677.158362 msecs"


;; (reduce + (xor-text-list cipher1 "god"))
;; 107359
(defn problem059 [] (reduce + (xor-text-list cipher1 "god")))




