Write a function which drops every Nth item from a sequence.
	
(= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
	
(= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
	
(= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])

(
# (filter #(> (mod % 3) 0) [1 2 3 4 5 6 7 8])

  (#(flatten (partition-all (dec %2) %2 %)) [1 2 3 4 5 6] 4)

  (#(mapcat drop-last (partition %2 %2 '(1) %1)) [1 2 3 4 5 6 7 8] 3)

  