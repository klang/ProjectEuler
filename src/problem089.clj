(ns problem089
  (:use [clojure.contrib.str-utils2 :only (split)])
  (:use clojure.contrib.duck-streams)
  (:use clojure.test))

(def roman-txt (slurp "roman.txt") )

(def roman-values (split roman-txt #"\r\n"))

