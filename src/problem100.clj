(ns p100
  (:use clojure.test))

(deftest test-explanations
  (is (= 1/2 (* (/ 15 21) (/ 14 20))))
  (is (= 1/2 (* (/ 85 120) (/ 84 119)))))

(defn condition [blue total]
  (= 1/2 (* (/ blue total) (/ (dec blue) (dec total)))))

(deftest test-condition
  (is (condition 15 21))
  (is (condition 85 120)))