# Write a function which duplicates each element of a sequence.
	
(= (reduce #(conj %1 %2 %2) [] [1 2 3]) '(1 1 2 2 3 3))
	
(= (reduce #(conj %1 %2 %2) [] [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
	
(= (reduce #(conj %1 %2 %2) [] [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
	
(= (reduce #(conj %1 %2 %2) [] [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))