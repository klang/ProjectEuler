(ns problem102
  (:use [clojure.string :only (split)]
        [tools.numbers :only (str2int)]
        [clojure.math.numeric-tower :only (abs)]))



(def triangles-txt (slurp "src/triangles.txt") )

(def triangles (map #(str2int %) 
		    (map #(split % #",") (split triangles-txt #"\r\n"))))

;; 


;; http://en.wikipedia.org/wiki/Slope-intercept_form#Two-point_form
(defn y [x1 y1 x2 y2] 
  "given two distinct points,
   find the function for the straight line going through them" 
  (fn [x] (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1)))))

(defn slope [x1 y1 x2 y2]
   (/ (- y2 y1) (- x2 x1)))

;; http://en.wikipedia.org/wiki/Slope-intercept_form#Slope.E2.80.93intercept_form
(defn slope-intercept-form [x1 y1 x2 y2]
  (fn [x] (+ (* (slope x1 y1 x2 y2) x) ((y x1 y1 x2 y2) 0))))

;; dot product
(defn dot [X-vector Y-vector]
  (map * X-vector Y-vector))

;; determinant
(defn det [[ux uy] [vx vy]]
  (- (* ux vy) (* uy vx)))

(defn a [[vx vy] 
	 [v0x v0y] [v1x v1y] [v2x v2y]]
  (/ (- (det [vx vy] [v2x v2y]) 
	(det [v0x v0y] [v2x v2y]))
     (det [v1x v1y] [v2x v2y])))

(defn b [[vx vy] 
	 [v0x v0y] [v1x v1y] [v2x v2y]]
  (- (/ (- (det [vx vy] [v1x v1y]) 
	   (det [v0x v0y] [v1x v1y]))
	(det [v1x v1y] [v2x v2y]))))

;; a, b > 0 and a+b<1
(defn interior [vx vy [v0x v0y v1x v1y v2x v2y]]
  (let [a1 (a [vx vy] [v0x v0y] [v1x v1y] [v2x v2y])
	b1 (b [vx vy] [v0x v0y] [v1x v1y] [v2x v2y])]
    (and (< 0 a1) (< 0 b1) (< (+ a1 b1) 1))))

;http://www.blackpawn.com/texts/pointinpoly/default.html
;(count (filter true? (map #(interior 0 0 %) triangles)))

;; Calculate the result in at least two different ways:
;; "DotProduct"
;; "Angle"
;; "Same Side"
;; "Area"

;; http://mathforum.org/library/drmath/view/54735.html
(defn area [[ x1 y1 x2 y2 x3 y3]]
  (* 1/2
     (+ (- (* x2 y1)) (* x3 y1) (* x1 y2)
	(- (* x3 y2)) (- (* x1 y3)) (* x2 y3))))

(defn inside-by-area [vx vy [v0x v0y v1x v1y v2x v2y]]
  (let [pp1p2 (abs (area [vx vy v0x v0y v1x v1y]))
	pp2p3 (abs (area [vx vy v1x v1y v2x v2y]))
	pp3p1 (abs (area [vx vy v2x v2y v0x v0y]))
	p1p2p3 (abs (area [v0x v0y v1x v1y v2x v2y]))]
    (= p1p2p3 (+ pp1p2 pp2p3 pp3p1))))

;; (count (filter true? (map #(inside-by-area 0 0 %) triangles)))
;; 228

(defn inside-by-same-side [vx vy [a1 a2 b1 b2 c1 c2]]
  ;; cross product of 
  ;; v0 - v1 and v - v1
  ;; v0 - v1 and v - v1
  ;; v0 - v1 and v - v1
  )

(defn inside-by-cross-product [[a1 a2 b1 b2 c1 c2]]
  (let [x1 (<= 0 (det [a1 a2] [b1 b2]))
	x2 (<= 0 (det [b1 b2] [c1 c2]))
	x3 (<= 0 (det [c1 c2] [a1 a2]))]
    (and (= x1 x2) (= x2 x3))
    )
  )

;; (count (filter true? (map #(inside-by-cross-product %) triangles)))
;; 228
(defn problem102 [] (count (filter true? (map #(inside-by-cross-product %) triangles))))
