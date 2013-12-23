"leeeeroy"

(loop
  [result '() x '(3 2 2 3)]
  (if (not (= (last result) (first x)))
    (cons (first x) result)
    (recur result (rest x))))

( #(loop
    [result '() x %]
    (if (empty? x)
    result
    (recur
     (if (= (last result) (first x))
       result
       (concat result (cons (first x) '())))
     (rest x)))) [[1 2] [1 2] [3 4] [1 2]])

(cons (first '(1 1 2 2 3)) '())

(reduce #(if (= (peek %1) %2) %1 (conj %1 %2)) '[] '(1 1 2 2 3))