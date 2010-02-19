;; Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
;; 
;; For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938  53 = 49714.
;; 
;; What is the total of all the name scores in the file?

(use '[clojure.contrib.str-utils2 :only (split)]
     'clojure.contrib.duck-streams
     'clojure.contrib.test-is)
(require '[clojure.contrib.str-utils2 :as s])

(defn word-value [w]  
  (reduce + (map #(- (int %) 64) w)))

(def names-txt (slurp "names.txt"))

(def problem022
     (map #(* (word-value %1) %2) 
	  (sort (map #(s/replace % "\"" "") 
		     (split names-txt #","))) 
	  (iterate inc 1)))

(deftest test-problem022
  (is (= "COLIN" (nth (sort (map #(s/replace % "\"" "") (split names-txt #","))) 937)))
  (is (= 53 (word-value "COLIN")))
  (is (= 49714 (nth problem022 937))))

(run-tests)

;;user> (reduce + problem022)
;;871198282
