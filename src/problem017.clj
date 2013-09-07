(ns problem017
  (meta {:description "If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage."})
  (:use 
   [clojure.test :only (deftest is)])
  (:require 
   [tools.numbers :only (digits) :as tools]))

(def british 
     {:words  
      {1 "one" 2 "two" 3 "three" 4 "four" 5 "five" 6 "six" 7 "seven" 8 "eight" 
       9 "nine" 10 "ten" 11 "eleven" 12 "twelve" 13 "thirteen" 14 "fourteen" 
       15 "fifteen" 16 "sixteen" 17 "seventeen" 18 "eighteen" 19 "nineteen" 
       20 "twenty" 30 "thirty" 40 "forty" 50 "fifty" 60 "sixty" 70 "seventy" 
       80 "eighty" 90 "ninety" 100 "hundred" 1000 "thousand"}
      :block
      {0 "hundred" 2 "thousand" 3 "million" 4 "billion"}})
;; TODO: hundreds should be at index 1

(defn british-english-number [n]
  (let [words (british :words)
	block (british :block)
	number (map #(reverse %) (reverse (partition 3 3 (repeat 0) (reverse (tools/digits n)))))
	blocks (count number)
	;; number will be split in blocks of 3, padded to the left with zeroes
	]
    ;; each block of 3 numbers are processed after the same model
    (loop [digit-blocks number, l [] ]
      (if (nil? (first digit-blocks))
	(apply str l)
	(let [ddd                  (first digit-blocks) 
	      x100                 (nth ddd 0)
	      x10                  (nth ddd 1)
	      x1                   (nth ddd 2)
	      _number-of-hundreds  (words x100)
	      _number-of-tens      (if (< 1 x10) 
				     (concat (words (* 10 x10)) (words x1)) ;; 20 <= 99
				     (words (+ (* 10 x10) x1)))             ;;  1 <= 19
	      _AND                 (if (and (< 0 x100) (< 0 (+ (* 10 x10) x1))) "and" nil)
	      _HUNDRED             (if (< 0 x100) (block 0))
	      _GAZILLION           (block (count digit-blocks))
	      ]
	  (recur (rest digit-blocks) 
		 (concat l _number-of-hundreds _HUNDRED _AND _number-of-tens _GAZILLION)
		 )
	)))))

(deftest bntest
(is (= 23 (count (british-english-number 342)))
    (= 20 (count (british-english-number 115)))))
;(run-tests)
(defn problem017 []
  (count (apply str (map #(british-english-number %) (range 1 1001)))))
;;21224 .. wrong .. spelling error
;;21124
