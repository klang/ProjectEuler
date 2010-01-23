;; Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

;; Triangle	 	Tn=n(n+1)/2	 	1, 3, 6, 10, 15, ...
;; Pentagonal	 	Pn=n(3n1)/2	 	1, 5, 12, 22, 35, ...
;; Hexagonal	 	Hn=n(2n1)	 	1, 6, 15, 28, 45, ...
;; It can be verified that T285 = P165 = H143 = 40755.

;; Find the next triangle number that is also pentagonal and hexagonal.

(use 'clojure.contrib.test-is)

;; lazy sequences
(defn triangles [] 
  (map (fn [n] (quot (* n (+ n 1)) 2)) (iterate inc 1)))
(defn pentagonals [] 
  (map (fn [n] (quot (* n (- (* 3 n) 1)) 2)) (iterate inc 1)))
(defn hexagonals [] 
  (map (fn [n] (* n (- (* 2 n) 1))) (iterate inc 1)))

;; as functions
(defn triangle [n] (quot (* n (+ n 1)) 2))
(defn pentagonal [n] (quot (* n (- (* 3 n) 1)) 2))
(defn hexagonal [n] (* n (- (* 2 n) 1)))

(use 'clojure.set)
(def tri (set (take 100000 (triangles))))
(def pen (set (take 100000 (pentagonals))))
(def hex (set (take 100000 (hexagonals))))
(intersection tri pen hex)
; #{1 1533776805 40755}

(deftest test-problem045
  (is (= 40755 (triangle 285) (pentagonal 165) (hexagonal 143))
      (= 1533776805 (triangel 55385) (pentagonal 31977) (hexagonals 27693))))

;; (run-tests)