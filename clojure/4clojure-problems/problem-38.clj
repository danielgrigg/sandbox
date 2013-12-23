Write a function which takes a variable number of parameters and returns the maximum value.
	
(= (__ 1 8 3 4) 8)
	
(= (__ 30 20) 30)
	
(= (__ 45 67 11) 67)
Special Restrictions
max
max-key

((fn
  [x & ys]
  (println "x " x " and ys " ys)) 3 4 5)

((fn
  [x & ys]
  (reduce #(if (> % %2) % %2) x ys)) 3 4 5 -7 9)