(println (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader)))) # classpath

(assoc-in {:a {:aa "foo"} :b "b" :c "c"} [:a :aa] "foo") 

(binding [*print-dup* true] (prn [1 2 3])) # serialize data
(read (PushbackReader. (StringReader. y)) # deserialize

(defmacro bench [& exprs] `(time (dotimes [~'_ 1E6] (do ~@exprs))))  # benchmark

      (/ (.maxMemory (java.lang.Runtime/getRuntime)) 1024.0 1024.0)
