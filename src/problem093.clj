(ns problem093
  (meta {:description "By using each of the digits from the set, {1, 2, 3, 4}, exactly once, and making use of the four arithmetic operations (+, , *, /) and brackets/parentheses, it is possible to form different positive integer targets.

For example,

8 = (4 * (1 + 3)) / 2
14 = 4 * (3 + 1 / 2)
19 = 4 * (2 + 3) - 1
36 = 3 * 4 * (2 + 1)

Note that concatenations of the digits, like 12 + 34, are not allowed.

Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 is the maximum, and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.

Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive integers, 1 to n, can be obtained, giving your answer as a string: abcd."})
  (:use 
   [tools.numbers :only (integer)]
   [clojure.math.combinatorics :only (permutations selections) :as comb]))

(comment
  ;; the 4 different operater graphs it is possible to make
  (a (b m (c n o)) p)
  (a m (b n (c o p)))
  (a m (b (c n o) p))
  (a (b (c m n) o) p))

(defn formular-1 [[a b c] [m n o p]]
  (try (a (b m (c n o)) p)
       (catch ArithmeticException _ 0)))
(defn formular-2 [[a b c] [m n o p]]
  (try (a m (b n (c o p)))
       (catch ArithmeticException _ 0)))
(defn formular-3 [[a b c] [m n o p]]
  (try (a m (b (c n o) p))
       (catch ArithmeticException _ 0)))
(defn formular-4 [[a b c] [m n o p]]
  (try (a (b (c m n) o) p)
       (catch ArithmeticException _ 0)))

(defn f [formular numbers]
  (distinct
   (sort
    (for [operators (selections [+ - * /] 3)
	  numbers (permutations numbers)
	  :let [n (formular operators numbers)]
	  :when (and (integer? n) (< 0 n))]
      n))))

(defn line [numbers]
  (distinct
   (sort
    (flatten
     (merge
      (f formular-1 numbers)
      (f formular-2 numbers)
      (f formular-3 numbers)
      (f formular-4 numbers))))))

(defn line-length [& numbers]
  (count (filter true? (map = (line numbers) (iterate inc 1)))))


(defn search [min max]
  (for [a (range min max) b (range min max) c (range min max) d (range min max)
	:when (< a b c d)] {:line [a b c d] :length (line-length a b c d)}))


(defn maximum [max]
  (loop [l (search 1 max)
	 m {:line [] :length 0}]
    (if (empty? l)
      m
      (recur (rest l) (if (< (:length m) (:length (first l)))
			(first l)
			m)))))

;; problem093> (time (maximum 10)) 
;; "Elapsed time: 14724.167179 msecs"
;; {:line [1 2 5 8], :length 51}
;; problem093> (time (maximum 15)) 

(defn problem093 [] (integer (:line (maximum 10))))


