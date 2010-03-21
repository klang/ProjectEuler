(use 'clojure.contrib.math 'clojure.contrib.test-is)
(load "tools")

;; stolen method:
;; integer-length defined as a private function in clojure.contrib.math
; Length of integer in binary, used as helper function for sqrt.
(defmulti #^{:private false} integer-length class)
(defmethod integer-length java.lang.Integer [n]
  (count (Integer/toBinaryString n)))
(defmethod integer-length java.lang.Long [n]
  (count (Long/toBinaryString n)))
(defmethod integer-length java.math.BigInteger [n]
  (count (. n toString 2)))

