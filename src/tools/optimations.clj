(defn foo [n]
  (loop [i 1]
    (if (< i n)
      (recur (inc i))
      i)))
 
;; (time (foo 100000))
;; "Elapsed time: 1.428 msecs"
;; 100000

(defn foo2 [n]
  (let [n (int n)]
    (loop [i (int 0)]
      (if (< i n)
        (recur (inc i))
        i))))
 
;; (time (foo2 100000))
;; "Elapsed time: 0.032 msecs"
;; 100000
;; 
;; user> (time (foo2 40000000))
;; "Elapsed time: 104.724805 msecs"
;; 40000000
;; user> (time (foo 40000000))
;; "Elapsed time: 7295.805593 msecs"
;; 40000000

(defn init-int-array-iterated [n]
  (let [n (int n) v (int-array n)]
    (loop [i (int 0)]
      (if (< i n)
        (do (aset v i i) (recur (inc i)))
        v))))

;; user> (time  (def v (int-array 10000000 (iterate inc 0))))
;; "Elapsed time: 13549.000418 msecs"

;; user> (time (def w (init-int-array-iterated 10000000)))
;; "Elapsed time: 853.208432 msecs"

;; user> (time  (def v (int-array 10000000 (iterate inc (int 0)))))
;; "Elapsed time: 4755.617789 msecs"

;; user> (time  (def v (int-array 10000000 (iterate (fn [x] (+ 1 x)) (int 0)))))
;; "Elapsed time: 8506.960126 msecs"
