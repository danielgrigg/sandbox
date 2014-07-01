Write a higher-order function which flips the order of the arguments of an input function.
	
(= 3 ((__ nth) 2 [1 2 3 4 5]))
	
(= true ((__ >) 7 8))
	
(= 4 ((__ quot) 2 8))
	
(= [1 2 3] ((__ take) [1 2 3 4 5] 3))

(((fn [f & xs] (apply f (reverse xs))) nth) 2 [1 2 3 4 5])

((fn [f & xs] (list xs)) map 2 3)

(defn my-fun [f] (fn [& xs] (apply f (reverse xs))))

((my-fun nth) 2 [1 2 3 4 5])
((my-fun take) [1 2 3 4 5] 3)

#(fn ([& s] (apply % (reverse s))))
