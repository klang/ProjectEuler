(ns tools-parts
  (:use clojure.test))

;; Miranda code from 
;; "Introduction to Functional Programming" by Richard Bird & Philip Wadler

;; p54
(defn ++ [a b] 
  "Concatenation take two lists and produces a third list"
  (vec (concat a b)))

(deftest test-pp
  (let [xs [1 2 3 4] ys [5 6 7] zs [8 9]]
    (is (= (++ (++ xs ys) zs) (++ xs (++ ys zs))))
    (is (= (++ [] xs) (++ xs []) xs))
    ;; the pure clojure versions
    (is (= (list 1 2 3 4 5 6) (concat (list 1 2 3) (list 4 5 6))
	   '(1 2 3 4 5 6) (concat '(1 2 3) '(4 5 6))))
    (is (= '(1 2 3 4 5 6 7 8 9) 
	   (concat (concat'(1 2 3) '(4 5 6)) '(7 8 9))
	   (concat '(1 2 3) (concat '(4 5 6) '(7 8 9)))
	   (concat '(1 2 3) '(4 5 6) '(7 8 9))))))
;; p54
(defn mconcat [colls]
  "concatenates a collection of collections into one list"
  (vec (reduce concat colls)))

(deftest test-mconcat
  (is (= [1 2 3 2 1] (mconcat [[1 2] [] [3 2 1]]) 
	 (vec (reduce concat [[1 2] [] [3 2 1]])))
      ;; the pure clojure version
      (= '(1 2 3 2 1) 
	 (reduce concat '((1 2) () (3 2 1))))))

;; p55
(defn hd [coll] 
  "selects the first element of a list and returns it as a list"
  (first coll))

(deftest test-hd 
  (let [x 1 xs [2 3]] 
    (is (= (hd (++ [x] xs)) x))
    ;; the pure clojure version
    (is (= (first (concat [x] xs)) x))))

(defn tl [coll] 
  "selets the remainding portion (the rest of the list) and returns it as a list"
  (vec (rest coll)))

(deftest test-tl 
  (let [x 1 xs [2 3]] 
    (is (= (tl (++ [x] xs)) xs))
    ;; the pure clojure version
    (is (= (rest (concat [x] xs)) xs))))

;(:)
(defn mcons [x seq]
  (vec (cons x seq)))
(deftest test-mcons
  (is (= [1] (mcons 1 [])))
  (is (= [1 2 3 4] (mcons 1 (mcons 2 [3 4]))))
  (is (= [\h \e \l \l \o]
	 (mcons \h (mcons \e (mcons \l (mcons \l (mcons \o [])))))))
  ;; the pure clojure version
  (is (= '(1) (cons 1 '())))
  (is (= '(1 2 3 4) (cons 1 (cons 2 [3 4]))))
  (is (= '(1 2 3 4) (cons 1 (cons 2 (range 3 5))))))

;; p135
;; parts []           = [[]]
;; parts [x]          = [[[x]]]
;; parts (x: (x':xs)) = map (glue x) (parts (x':xs)) ++ map ([x]:) (parts (x':xs))

;; glue x xss         = (x:hd xss):tl xss

;; TODO: figure out what the above notation means..
(defn glue [x xss] (cons (cons x (hd xss)) (tl xss)))
(defn glue [x xss] (vec (concat [x] xss)))
(defn glue [x xss] (concat [x] xss))

(defn parts [x]
  (cond (empty? x) [[]]
	(= (count x) 1) [x]
	:else (into (vec (map #(glue (first x) %) (parts (rest x))))
		      (map #(cons (first x) %) (parts (rest x))))
	))

