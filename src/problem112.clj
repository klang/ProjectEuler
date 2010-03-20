(use 'clojure.contrib.test-is)
(load "tools")

(defn digits [number]
  "convert number to digit list"
  (cond (zero? number) (list 0)
	:else
	(loop [n number dl ()]
	  (if (zero? n) dl
	      (recur (quot n 10) (conj dl (rem n 10)))))))

(defn increasing [number]
  (loop [n (digits number) r true]
    (if (or (not r) (= 1 (count n))) r
	(recur (rest n) (and (<= (first n) (second n)) r)))))

(defn decreasing [number]
  (loop [n (digits number) r true]
    (if (or (not r) (= 1 (count n))) r
	(recur (rest n) (and (>= (first n) (second n)) r)))))

(deftest test-increasing
  (is (increasing 134468))
  (is (not (increasing 66420))))

(deftest test-decreasing
  (is (decreasing 66420))
  (is (not (decreasing 134468))))

(defn bouncy [number]
  (not (or (increasing number) (decreasing number)) ))

(defn bouncy [number]
  (and (not (increasing number)) (not (decreasing number))))

(defn bouncy [number]
  (loop [n (digits number) i true d true]
    ;; by checking for bouncyness ever time we can return quicker
    (if (or (not (or i d)) (= 1 (count n))) (not (or i d))
	(recur (rest n) 
	       (and (<= (first n) (second n)) i)
	       (and (>= (first n) (second n)) d)))))


(deftest test-bouncy
  (is (bouncy 155349))
  (is (not (bouncy 66420)))
  (is (not (bouncy 134468))))

(defn bouncy-count [limit]
  (loop [b 0 n 1]
    (if (< limit n) b 
      (if (bouncy n)
	(recur (inc b) (inc n))
	(recur b (inc n))))))

(deftest test-bouncy-count
  (is (= 525 (bouncy-count 1000)))
  (is (= 1/2 (/ (bouncy-count 538) 538)))
  (is (= (* 2 (f 538)) 538)))

(defn bouncy-50-percentage []
  (loop [b 0 n 100]
    (if (or (= (* 2 b) (- n 1)) (< 1000 n)) (list b (- n 1)) 
	(if (bouncy n)
	  (recur (inc b) (inc n))
	  (recur b (inc n))))))

(defn bouncy-50-percentage []
  (loop [b 0 n 100]
    (if (or (= (* 100 b) (* 50 (- n 1))) (< 1000 n)) (list b (- n 1)) 
	(if (bouncy n)
	  (recur (inc b) (inc n))
	  (recur b (inc n))))))

(defn bouncy-90-percentage []
  (loop [b 0 n 100]
    (if (= (* 100 b) (* 90 (- n 1))) (list b (- n 1)) 
	(if (bouncy n)
	  (recur (inc b) (inc n))
	  (recur b (inc n))))))

(deftest test-bouncy-90-percentage
  (is (= 21780 (second (bouncy-90-percentage)))))

(defn bouncy-99-percentage []
  (loop [b 0 n 100]
    (if (= (* 100 b) (* 99 (- n 1))) (list b (- n 1)) 
	(if (bouncy n)
	  (recur (inc b) (inc n))
	  (recur b (inc n))))))

;; user> (time (bouncy-99-percentage))
;; "Elapsed time: 15708.773854 msecs"
;; (1571130 1587000)
