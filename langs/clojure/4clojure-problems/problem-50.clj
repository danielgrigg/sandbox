Write a function which takes a sequence consisting of items with different types and splits them up into a set of homogeneous sub-sequences. The internal order of each sub-sequence should be maintained, but the sub-sequences themselves can be returned in any order (this is why 'set' is used in the test cases).
	
(= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
	
(= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
	
(= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})

(fn [x]
  (let [p #(str (type %))]
    (partition-by p (sort-by p x)))

((fn [x]
  (let [p #(str (type %))]
    (partition-by p (sort-by p x)))) [1 :a 2 :b 3 :c])

(partition-by (fn [x] (type x)) (sort-by #(str (type %)) [[1 2] :a [3 4] 5 6 :b]))

((fn [x] partition-by (fn [y] (str (type y))) (sort-by #(str (type %)) x)) [1 :a 2 :b 3 :c])

(defn split-by-type
  [x]
  (partition-by (fn [x] (type x)) (sort-by #(str (type %)) [[1 2] :a [3 4] 5 6 :b])))

((fn [x] (partition-by #(type %) (sort-by #(str (type %))))) [[1 2] :a [3 4] 5 6 :b])
