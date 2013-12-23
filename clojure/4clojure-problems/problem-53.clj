Longest Increasing Sub-Seq
Difficulty:	Hard
Topics:	seqs

Given a vector of integers, find the longest consecutive sub-sequence of increasing numbers. If two sub-sequences have the same length, use the one that occurs first. An increasing sub-sequence must have a length of 2 or greater to qualify.
	
(= (biggest-monotonic-seq [1 0 1 2 3 0 4 5]) [0 1 2 3])
	
(= (biggest-monotonic-seq [5 6 1 3 2 7]) [5 6])
	
(= (biggest-monotonic-seq [2 3 3 4 5]) [3 4 5])
	
(= (biggest-monotonic-seq [7 6 5 4]) [])

(defn sub-sequences
  [x]
  (filter (fn [c] (>= (count c) 2))
	  (partition-by
	   (fn [[a b]] (< b a))
	   (map #(vector % %2) x (rest x)))))

(distinct (flatten (first (sub-sequences [1 0 1 2 3 0 4 5]))))

((defn big-subseq [x]
   (let [p (fn [[a b]] (< a b))]
	 (distinct
	  (flatten (first
		    (sort-by #(count %) >
			     (partition-by
			      (fn [[a b]] (< b a))
			      ( partition 2 1 x)))))))) [1 0 1 2 3 0 4 5])
 
(filter (fn [x] (> (count x) 1)) (partition-by (fn [[a b]] (< b a)) (partition 2 1 [1 0 1 2 3 0 4 5])))

(filter (fn [x] (> (count x) 1)) (partition-by (fn [[a b]] (< b a)) (partition 2 1 [7 6 5 4])))
; 1, 0 1 2 3, 0 4 5
(let [x [1 0 1 2 3 0 4 5]] (map #(vector %2 (- %2 %)) x (rest x)))

(map (fn [[a b]] (- b a)) (partition 2 1 [1 0 1 2 3 0 4 5]))

(let [x [1 0 1 2 3 0 4 5]] (map #(vector % (-% %2)) x (cons 0 x)))

(let [x [7 6 5 4 1 0 1 2 3 0 4 5]]
  (let [ss (map (comp distinct flatten)
		(partition-by (fn [[a b]] (< b a)) (partition 2 1 x)))]
    (first (sort-by #(count %) >
	   (filter (fn [t] (pos? (reduce #(- %2 %) t))) ss)
	   ))))

(let [x [2 3 3 4 5]]
  (partition-by (fn [[a b]] (<= b a)) (partition 2 1 x)))

; Solution here!
((fn [x]
 (let [ss (map (comp distinct flatten)
		(partition-by (fn [[a b]] (<= b a)) (partition 2 1 x)))]
    (concat (first (sort-by #(count %) >
	   (filter (fn [t] (pos? (reduce #(- %2 %) t))) ss)
	   )) []))) [1 0 1 2 3 0 4 5])

					; andy's solution
(fn [[h & r]]
  (or
   (last
    (filter #(> (count %) 1)
	    (sort-by count
		     (reduce (fn [[h & r :as a] e]
			       (if (< (peek h) e)
				 (conj r (conj h e))
				 (conj a [e]))) (list [h]) r))))
   []))
