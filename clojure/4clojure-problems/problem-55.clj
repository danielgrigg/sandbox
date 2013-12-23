Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
	
(= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
	
(= (__ [:b :a :b :a :b]) {:a 2, :b 3})
	
(= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})

(frequencies [1 1 2 3 2 1 1])

(defn soln [x]
  (mapcat #(conj {} {(first %) (count %)}) (partition-by identity (sort x))))
(soln [:b :a :b :a :b])
(soln '([1 2] [1 3] [1 3]))

(map #(conj {} {(first %) (count %)}) (partition-by identity (sort '([1 2] [1 3] [1 3]))))

;solution!
(fn [c]
  (into {}
   (for [x (partition-by identity (sort c))] [(first x) (count x)])))


(reduce {} #(conj % {(first %2) (count %2)}) '((1 1 1 1)))
(reduce {} #(conj % {%2 8}) [1 2 3 4])
(mapcat #(conj {} {(first %) (count %)}) '((1 1 1) (2 2) (3)))

(conj (conj {} {1 8}) {2 8})
(map #(conj {} {(first %) (count %)}) '((1 1 1 1)))
(conj {} {(first '(2 2)) (count '(2 2))})
(cons {2 3} {1 5})
({:a 3 :b 4} :c)
((conj {} {:a 3} {:b 4}) :b)