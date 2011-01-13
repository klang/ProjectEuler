(ns problem317
  (meta {:description "A firecracker explodes at a height of 100 m above level ground. It breaks into a large number of very small fragments, which move in every direction; all of them have the same initial velocity of 20 m/s.

We assume that the fragments move without air resistance, in a uniform gravitational field with g=9.81 m/s².

Find the volume (in m³) of the region through which the fragments move before reaching the ground. Give your answer rounded to four decimal places."
	 :hints ";; http://en.wikipedia.org/wiki/Trajectory#Example:_Constant_gravity.2C_no_drag_or_wind"})
  (:use [clojure.contrib.math :only (expt)]))



;; y_0 = 100
;; v_0 = 20
;; g = 9.81

;; parabola of safty
;; y_0 + v_0² / (2 g) - g x² / (2 v_0²) - y = 0

;; plugged into wolfram|alpha

;; http://www.wolframalpha.com/input/?i=100+%2B+20%C2%B2+/+(2+*+9.81)+-+9.81+x%C2%B2+/+(2*20%C2%B2)+-+y+%3D+0

;; -0.0122625 (-99.08340778978892 + x) (99.08340778978892 + x) - y == 0

(defn R
  "The range, R, is the greatest distance the object travels along the x-axis in the I sector"
  [initial-velocity initial-angle gravity]
  (/ (* (expt initial-velocity 2) (Math/sin (* 2 initial-angle))) gravity))

;; as we have some elevation in the picture, we really can not use this
;; range formular

(defn h
  [initial-velocity initial-angle gravity]
  (/ (* (expt initial-velocity 2) (Math/sin initial-angle)
	(Math/sin initial-angle)) (* 2 gravity)))

;; the maximum height is reached at
;; (h 20 (/ (Math/PI) 2) 9.81)
;; or simply at h-max
(defn h-max [v g]
  (/ (* 2 v v) (* 4 g)))

(def height (+ 100 (h-max 20 9.81)))
(def radius 99.08340778978892)
(def volume (* 0.5 Math/PI radius radius height))
;; 1856532.8455[27574]