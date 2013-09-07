(ns tools.misc)

(defn flatten-once [s] (remove seq? (tree-seq seq? seq s)))
(defn indexed [s] (map vector (iterate inc 0) s))
