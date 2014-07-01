Write a function which calculates factorials.
	
(= (__ 1) 1)
	
(= (__ 3) 6)
	
(= (__ 5) 120)
	
(= (#(reduce * % (range 1 %)) 8) 40320)

(reduce * 8 (range 1 8))

#(reduce * % (range 1 %))