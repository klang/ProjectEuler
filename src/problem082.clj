(ns problem082
  (meta {:description "The minimal path sum in the 5 by 5 matrix below, by starting in any cell in the left column and finishing in any cell in the right column, and only moving up, down, and right, is indicated in red and bold; the sum is equal to 994.

201 96 342 234 103 18 = 994

Find the minimal path sum, in matrix.txt, a text file containing a 80 by 80 matrix, from the left column to the right column"})
  (:use 
   [clojure.test :only (deftest is)]
   [tools.dijkstra :only (shortest-path)]
   [problem081 :only (str2int read-data)]
   [problem083 :only (children distance marker pos problem node up down right)])
  (:require
   [clojure.string :only (split) :as str]))

(def matrix (read-data "src/matrix.txt"))

(def example [[131 673 234 103  18]
	      [201  96 342 965 150]
	      [630 803 746 422 111]
	      [537 699 497 121 956]
	      [805 732 524  37 331]])

(defn add-source-and-sink-flipped [matrix]
  (let [m (count matrix)]
    (merge
     {:source (into {} (map #(hash-map (marker [% 0]) (pos matrix [% 0])) (range 0 m)))}
     (into {}
	   (map #(hash-map (marker [% (dec m)])
			   {:sink 0}) (range 0 m))))))

(defn nodes82 [matrix]
  (let [maxij (count matrix)]
    (merge-with merge
		(add-source-and-sink-flipped matrix)
		(into {} (for [i (range 0 maxij) j (range 0 maxij)]
			   {(marker [i j]) (node matrix [i j] up down right)})))))

(deftest test-nodes
  (is (= 994
	 (second (shortest-path (merge-with
				 merge
				 (nodes82 example)
				 (add-source-and-sink-flipped example))
				:source :sink children distance)))))

(defn problem082 []
  (second (problem matrix nodes82)))

;;(time (problem082))
;;"Elapsed time: 691.370197 msecs"
;;260324
