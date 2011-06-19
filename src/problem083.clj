(ns problem083
  (meta {:description "In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right, by moving left, right, up, and down, is indicated in bold red and is equal to 2297.

131 201 96 342 234 103 18 150 111 422 121 37 331 = 2297
"})
  (:use [problem081 :only (str2int read-data)])
  (:use clojure.test)
  (:use [clojure.contrib.str-utils2 :only (split)])
  (:use [tools.dijkstra :only (shortest-path)]))

(def matrix (read-data "src/matrix.txt"))

(def example [[131 673 234 103  18]
	      [201  96 342 965 150]
	      [630 803 746 422 111]
	      [537 699 497 121 956]
	      [805 732 524  37 331]])

;; to use dijkstra
(defn children [net node]
  (keys (net node)))

(defn distance [net nodesrc nodedst]
  ((net nodesrc) nodedst))

;; converting a matrix to a graph
(defn marker
  "returns a keyword :nij for the node in point (i,j)"
  [[i j]] (keyword (str "n" i "-" j)))

(defn pos   [matrix [i j]] (nth (matrix i) j))
(defn up    [[i j]] [(dec i) j])
(defn down  [[i j]] [(inc i) j])
(defn left  [[i j]] [i (dec j)])
(defn right [[i j]] [i (inc j)])

(deftest test-directions
  (is (= 96 (pos example [1 1])))
  (is (= 673 (pos example (up [1 1]))))
  (is (= 803 (pos example (down [1 1]))))
  (is (= 201 (pos example (left [1 1]))))
  (is (= 342 (pos example (right [1 1])))))

(deftest test-directions
  (is (= [0 1] (up [1 1])))
  (is (= [2 1] (down [1 1])))
  (is (= [1 0] (left [1 1])))
  (is (= [1 2] (right [1 1]))))

(defn position?
  "returns true the position is within the matrix"
  [matrix [i j]]
  (every? #(and (< % (count matrix)) (or (pos? %) (zero? %))) [i j]))

(defn legal-directions
  [matrix [i j] directions]
  ;; the signature should be & directions, but how do we pass into this function from 'node?
  (filter #(position? matrix %) (map #(% [i j]) directions)))

(defn node
  [matrix [i j] & directions]
  (into {} (map #(hash-map (marker %) (pos matrix %))
		(legal-directions matrix [i j] directions))))

(defn problem
  ":source is top left (0,0), :sink is bottom right (79,79)"
  [matrix nodes]
  (shortest-path (merge-with merge
			     {:source {(marker [0 0]) (pos matrix [0 0])}}
			     (nodes matrix)
			     {(marker [(dec (count matrix))
				       (dec (count (nth matrix 0)))]) {:sink 0}}
			     {:sink {}})
		 :source :sink children distance))

(defn nodes81 [matrix]
  (let [maxij (count matrix)]
    (into {} (for [i (range 0 maxij) j (range 0 maxij)]
	       {(marker [i j]) (node matrix [i j] down right)}))))

(defn nodes83 [matrix]
  (let [maxij (count matrix)]
    (into {} (for [i (range 0 maxij) j (range 0 maxij)]
	       {(marker [i j]) (node matrix [i j] up down left right)}))))

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
  (is (= 2427 (second (problem example nodes81))
	      (reduce + [131, 201, 96, 342, 746, 422, 121, 37, 331])))
  (is (= 994 (second (problem example nodes82))
	     (reduce + [201 96 342 234 103 18])))
  (is (= 994
	 (second (shortest-path (merge-with
				 merge
				 (nodes82 example)
				 (add-source-and-sink-flipped example))
				:source :sink children distance))))
  
  (is (= 2297 (second (problem example nodes83))))
  (is (= 427337 (second (problem matrix nodes81)))) ;; old answer for 81, 67 and 18
  (is (= 260324 (second (problem matrix nodes82)))) 
  (is (= 425185 (second (problem matrix nodes83)))) ;; calculated in python first
  )

(defn problem083 []
  (second (problem matrix nodes83)))

;;(time (problem083))
;;"Elapsed time: 701.88803 msecs"
;;425185

