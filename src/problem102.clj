(use '[clojure.contrib.str-utils2 :only (split)]
     'clojure.contrib.duck-streams)
(require '[clojure.contrib.str-utils2 :as s])


(def triangles-txt (slurp "triangles.txt") )

(defn str2int [v]
  (map #(. Integer parseInt % 10) v))

(def triangles (map #(str2int %) 
		    (map #(split % ",") (split triangles-txt #"r\n"))))


(defn triangle-origin [[x1 y1 x2 y2 x3 y3]] nil)
