Write a function which can rotate a sequence in either direction.
	
(= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
	
(= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
	
(= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
	
(= (rotate 1 '(:a :b :c)) '(:b :c :a))
	
(= (rotate -4 '(:a :b :c)) '(:c :a :b))

(fn [n x] (mod n (count x)))

(defn rotate
  [n x]
  (let [q (count x) p (mod n q)]
    (concat (take-last (- q p) x) (take p x))))

(rotate 6 [1 2 3 4 5])
(#(let [q (count %2) p (mod % q)]
    (concat (take-last (- q p) %2) (take p %2))) 6 [1 2 3 4 5])

#(let [c (count%2)] (take c (drop ( mod % c)(cycle %2))))