Write a function which allows you to create function compositions. The parameter list should take a variable number of functions, and create a function applies them from right-to-left.
	
(= [3 2 1] ((foo rest reverse) [1 2 3 4]))
	
(= 5 ((foo (partial + 3) second) [1 2 3 4]))
	
(= true ((foo zero? #(mod % 8) +) 3 5 7 9))
	
(= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))
Special Restrictions
comp

; hmm, weirdness here...
(defn foo [& xs]
  (fn [c] (reduce #(%2 %) c (reverse [xs]))))
((foo rest reverse) [1 2 3 4])

; test applying rest reverse to a collection
((fn [& xs] (reduce #(%2 %) [1 2 3 4] (reverse xs))) rest reverse)

					; ok, soln time - close over the function collection
(((fn [& xs] (fn [c] (reduce #(%2 %) c (reverse xs)))) rest reverse) [1 2 3 4])

; and presto
(fn [& xs] (fn [c] (reduce #(%2 %) c (reverse xs))))

					; easier to test when named
(defn foo [& xs] (fn [c] (reduce #(%2 %) c (reverse xs))))

(reduce #(%2 %) [1 2 3 4] (reverse [rest reverse]))


