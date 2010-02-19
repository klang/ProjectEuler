(defn sqube [[ p q]]
  (* (* p p) (* q q q)))

(map (fn [[p q]] (* (* p p) (* q q q))) 
     (combinations (take 5 primes) 2))

(defn sqube [[p q]]
  (let [a (* p p)
	b (* q q)]
    (list (* a p b) (* a q b))))

(map #(sqube %)
     (combinations (take 5 primes) 2))

(defn squbes [limit] 
  (sort (reduce concat 
		(map (fn [[p q]] (let [a (* p p) b (* q q)]
				   (list (* a p b) (* a q b)))) 
		     (combinations (take limit primes) 2)))))

(defn squbes-count [limit] (* 2 limit (- limit 1)))

(def prime-proof nil)

(defn substring-200 [number]
  (<= 0 (.indexOf (.toString number) "200")))

;; (filter #(substring-200 %) ( squbes 10))

;; user> (filter #(substring-200 %) (squbes 50))
;; (200 96632003 120072949 2005909453 2005964183 4823420099 20045656993 24152002213 34320079397 42239200781 200495610673 200733641363 200942019053 320064006697 372009061531)

;;user> (filter #(= 1992008 %) (squbes 50))
;;()
;; 1992008 is missing from this list .. why?