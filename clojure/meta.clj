; ^#' = meta-var-quote
; #^ =  metadata
(defn #^{:tag String} shout [#^{:tag String} s] (.toUpperCase s))

(defn #^String shout [#^String s] (.toUpperCase s))

(defn shout ([s] (.toUpperCase s)) {:tag String})

(def #^{:testdata true} foo (with-meta [1,2,3] {:order :ascending}))

