(use '[clojure.contrib.str-utils2 :only (split)]
     'clojure.contrib.duck-streams)
(require '[clojure.contrib.str-utils2 :as s])

(def roman-txt (slurp "roman.txt") )

(def roman-values (split roman-txt #"\r\n"))

