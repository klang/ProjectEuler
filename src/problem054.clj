(ns problem054
  (meta {:description ""})
  (:require [clojure.contrib.str-utils2 :as s])
  (:use clojure.contrib.duck-streams)
  (:use clojure.test))

(def poker-txt (slurp "poker.txt"))

;; (take 2 (s/split poker-txt #"\r\n"))
;; (take 5 (map #(s/split % #" ") (s/split poker-txt #"\r\n")))
;; {:p1 [8C TS KC 9H 4S] :p2 [7D 2S 5D 3S AC]}

;spades (♠), hearts (♥), diamonds (♦) and clubs (♣)
(def card-suites {:spades :S, :hearts :H, :diamonds :D :clubs :C})

(defn convert-card-representation [s]
  )

(def card-values {:2 1 :3 2 :4 3 :5 4 :6 5 :7 6 :8 7 :9 8 :10 9 
		  :Jack 10 :Queen 11 :King 12 :Ace 13 })
[{(card-values :8) C} {T S} {K C} {9 H} {4 S}]
;;(card-sort [8C TS KC 9H 4S])