(ns primes)
 
(defn- wheel2357 [] (cycle [2 4 2 4 6 2 6 4 2 4 6 6 2 6 4 2 6 4 6 8 4 2 4 2 4 8
 6 4 6 2 4 6 2 6 6 4 2 4 6 2 6 4 2 4 2 10 2 10]))
 
(defn- spin [l n] (lazy-seq (cons n (spin (rest l) (+ n (first l))))))
 
(defn- insert-prime [p xs table]
  (update-in table [(* p p)] #(conj % (map (fn [n] (* n p)) xs))))
 
(defn- reinsert [table x table-x]
  (loop [m (dissoc table x), elems table-x]
    (if-let [elems (seq elems)]
      (let [elem (first elems)]
    (recur (update-in m [(first elem)] #(conj % (rest elem))) (rest elems)))
      m)))
 
(defn- adjust [x table]
  (let [nextTable (first table),
    n (nextTable 0)]
    (if (<= n x) (recur x (reinsert table n (nextTable 1)))
    table)))
 
(defn- sieve-helper [s table]
  (when-let [ss (seq s)]
    (let [x (first ss), xs (rest ss),
      nextTable (first table),
      nextComposite (nextTable 0)]
      (if (> nextComposite x)
    (lazy-seq (cons x (sieve-helper xs (insert-prime x xs table))))
    (recur xs (adjust x table))))))
 
(defn- sieve [s]
  (when-let [ss (seq s)]
    (let [x (first ss), xs (rest ss)]
      (cons x (sieve-helper xs (insert-prime x xs
                         (sorted-map)))))))
(defn primes
  "(primes) creates a lazy stream of all positive prime numbers"
  []
  (concat [2 3 5 7] (sieve (spin (wheel2357) 11))))

;------------------------
