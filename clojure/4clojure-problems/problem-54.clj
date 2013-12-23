Partition a Sequence
Difficulty:	Medium
Topics:	seqs core-functions


Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
	
(= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
	
(= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
	
(= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
Special Restrictions
partition
partition-all

(map #(vector % %2) (range 9) (range 2 11))

(partition-by (fn [[a b]] (quot a 3)) '([0 2] [1 3] [2 4] [3 5] [4 6] [5 7] [6 8] [7 9] [8 10]))

(mapcat (map (fn [[x y]] y))
	(partition-by (fn [[a b]] (quot a 3)) '([0 2] [1 3] [2 4] [3 5] [4 6] [5 7] [6 8] [7 9] [8 10])))

(map #(map (fn [[a b]] b) %) '(([0 2] [1 3] [2 4]) ([3 5] [4 6] [5 7]) ([6 8] [7 9] [8 10])))

; soln
(defn foo [n x]
  (map #(map (fn [[a b]] b) %)
       (partition-by (fn [[c d]] (quot c n))
		     (map (fn [e f] (vector e f))
			  (range (count x))
			  (drop-last (rem (count x) n) x)))))

(foo 2 (range 8))
(foo 3 (range 8))

(range (count (range 8)))
(drop-last (rem 8 3) (range 8))
([0 1 2] 2)
