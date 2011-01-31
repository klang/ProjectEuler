(ns problem144
  (meta {:description ""
	 :url "http://www.wolframalpha.com/input/?i=4x%C2%B2%2By%C2%B2%3D100+and+y%3D((-9.6+-10.1)/1.4)x+%2B+10.1"})
  (:use [incanter core stats charts] )
  (:use [clojure.contrib.repl-utils])
  (:require [clojure.contrib.generic.math-functions :as generic])
  )
 
;(def pi Math/PI)
(def pi (* 4 (atan 1)))
(def dtor (/ pi 180))
(def rtod (/ 180 pi))
(def radians (/ pi 4))
(def degrees 45)

(comment
  (println (str (sin radians) " " (sin (* degrees dtor))))
  (println (str (cos radians) " " (cos (* degrees dtor))))
  (println (str (tan radians) " " (tan (* degrees dtor))))
  (println (str (asin (sin radians) ) " " (* (asin (sin (* degrees dtor))) rtod)))
  (println (str (acos (cos radians) ) " " (* (acos (cos (* degrees dtor))) rtod)))
  (println (str (atan (tan radians) ) " " (* (atan (tan (* degrees dtor))) rtod))))

(defn slope
  "returns the slope for the line going through the two points"
  [[x1 y1] [x2 y2]]
  (/ (- y2 y1) (- x2 x1)))

(defn two-point-form
  "returns the function for the line going through the two points"
  [[x1 y1] [x2 y2]]
  (fn [x] (+ (* (slope [x1 y1] [x2 y2]) (- x x1)) y1)))

(defn point-slope-form
  "retuns the function for the line going through the point with the slope given"
  [[x1 y1] slope]
  (fn [x] (+ (* slope (- x x1)) y1)))

(defn line
  ([x1 y1 x2 y2]
     (line x1 y1 (/ (- y2 y1) (- x2 x1))))
  ([x1 y1 slope]
     {:slope slope :fn (fn [x] (+ (* slope (- x x1)) y1))}))



(defn parametric-form
  "returns two functions describing the parametric form of the line
  going through the two points"
  [[x1 y1] [x2 y2]]
  [ (fn [t] (- (* (- x2 x1) t) x1))
    (fn [t] (- (* (- y2 y1) t) y1)) ])

(defn ellipse [x y]
  (= 100 (+ (* y y) (* 4(* x x))))
  )

(defn eslope [x y]
  (* -4 (/ x y)))

(defn tangent [x0 y0]
  (fn [x] (+ (* (slope x0 y0) (- x x0))y0)))

(defn exit? [[x y]] (and (<= -0.1 x 0.1) (<= 9.99998 y 10)))

(defn epoints [] [[0.0 10] [1.4 -9.6]])
(defn test-points [] [[0.0 10] [1.4 -9.6]
		      [-3.9906 -6.02499]
		      [0.323483 9.97891]
		      [0.41649 -9.96525]
		      [-4.47344 4.4669]
		      [1.13234 9.74819]])

;;(defn epointss [elipse linear-function] returns a sequence of reflection points)

(defn problem144 []
  (count (take-while #(not (exit? %)) (epoints))))

(defn point-slope-form [[x1 y1] m]
  (fn [x] (+ (* m (- x x1)) y1)))

(defn ellipse-top [x] (Math/sqrt (- 100 (* 4 (* x x)))))

(defn ellipse-bottom [x] (* -1 (Math/sqrt (- 100 (* 4 (* x x))))))

(defn tangent [[x1 y1]]
  (point-slope-form [x1 y1] (eslope x1 y1)))

(defn angle
  "m2 > m1"
  [m1 m2]
  (/ (- m2 m1) (+ 1 (* m1 m2))))

(defn normal-function [] )

(doto (function-plot ellipse-top -5 5)
  (add-function ellipse-top -0.01 0.01) ;; the hole
  (add-function ellipse-bottom -5 5)
  (add-points [1.4] [-9.6])             ;; the first reflection point
  (add-points [-3.9906] [-6.02499])     ;; the second reflection point
  #_(add-points [0.323483] [9.97891])   ;; 3rd
  #_(add-points [0.41649] [ -9.96525])  ;; 4th
  #_(add-points [-4.47344] [ 4.4669])   ;; 5th
  #_(add-points [1.13234] [9.74819])    ;; 6th
  (add-function (two-point-form [0,10.1] [1.4,-9.6]) -0 1.4) ;; first beam
  (add-function (tangent [1.4 -9.6]) 0 2.8)                  ;; tangent to ellipse
  #_(add-function (point-slope-form [1.4 -9.6] (angle (eslope 1.4 -9.6) (slope [0,10.1] [1.4,-9.6]))) 0 2.8)
  #_(add-function (point-slope-form [1.4 -9.6] (/ -1 (eslope 1.4 -9.6) )) 0 2.8)
  
  view)

(defn dotproduct
  "the dot product of two vectors a and b"
  [a b]
  (reduce + (map * a b)))

(defn reflection [l v]
  (- (* 2 (/ (dotproduct v l) (dotproduct l l)) l)) v)

