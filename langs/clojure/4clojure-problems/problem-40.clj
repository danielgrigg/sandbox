Write a function which separates the items of a sequence by an arbitrary value.
	
(= ((fn [x y] (mapcat #(conj [] % x) y)) 0 [1 2 3]) [1 0 2 0 3])
	
(= (apply str ((fn [x y] (mapcat #(conj [] % x) y)) ", " ["one" "two" "three"])) "one, two, three")
	
(= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])
Special Restrictions
interpose


((fn [x y] (drop-last (mapcat #(conj [] % x) y))) 0 [1 2 3])
