Write a function which replicates each element of a sequence a variable number of times.
	
(= ((fn [coll n] (reduce #(concat %1 (repeat n %2)) [] coll)) [1 2 3] 2) '(1 1 2 2 3 3))
	
(= ((fn [coll n] (reduce #(concat %1 (repeat n %2)) [] coll)) [:a :b] 4) '(:a :a :a :a :b :b :b :b))
	
(= ((fn [coll n] (reduce #(concat %1 (repeat n %2)) [] coll)) [4 5 6] 1) '(4 5 6))
	
(= ((fn [coll n] (reduce #(concat %1 (repeat n %2)) [] coll)) [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
	
(= ((fn [coll n] (reduce #(concat %1 (repeat n %2)) [] coll)) [44 33] 2) [44 44 33 33])

((fn [coll n] (reduce #(concat %1 (repeat n %2)) [] coll)) '(1 2 3) 2)

reduce #(conj %1 %2 %2) []

(