(use '[clojure.contrib.str-utils])
(filter (fn [w] (> (count w) 3)) (re-split #"\s+" "the\t quick  brown fox"))
;(filter (fn [w] (> (count w) 3)) (re-split #"\s+" "the\t quick  brown fox"))

