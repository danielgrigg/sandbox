Write a function which reverses the interleave process into x number of subsequences.
	
(= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
	
(= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
	
(= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

(defn indexer
  [x]
  (map #(conj [] % %2) (range (count x)) x))

(indexer '(1 :a 2 :b 3 :c))
(indexer (range 90 99))

((fn [x n] partition-by #(quot (first %) n) (indexer x)) (range 90 99) 3)

(take-nth 3 (range 1 9))
(drop 2 (range 9))

((fn [x n] (map #(take-nth n (drop % x)) (range n))) (range 9) 3)