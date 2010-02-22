(use '[clojure.contrib.str-utils2 :only (split)]
     'clojure.contrib.duck-streams)
(require '[clojure.contrib.str-utils2 :as s])


(def triangles-txt (slurp "triangles.txt") )

(defn str2int [v]
  (map #(. Integer parseInt % 10) v))

(def triangles (map #(str2int %) 
		    (map #(split % #",") (split triangles-txt #"\r\n"))))

;http://en.wikipedia.org/wiki/Linear_equation#Two-point_form
;; convert the two point form to the intercept form for each
;; of the 3 lines in each triangle. 
;; from that, it can be determined where the origin is in 
;; relation to the lines

(defn triangle-origin [[x1 y1 x2 y2 x3 y3]] nil)
