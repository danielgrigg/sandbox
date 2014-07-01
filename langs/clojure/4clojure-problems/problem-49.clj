Write a function which will split a sequence into two parts.
	
(= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
	
(= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
	
(= (#(list (take % %2) (drop % %2)) 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])

#(vector (take % %2) (drop % %2))

