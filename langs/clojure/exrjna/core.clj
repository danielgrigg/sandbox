(defmacro jna-call [lib func ret & args] 
  `(let [library#  (name ~lib)
           function# (com.sun.jna.Function/getFunction library# ~func)] 
           (.invoke function# ~ret (to-array [~@args]))))

(defn solid-framebuffer [w h c]
      (repeat (* w h) c))

(defn native-framebuffer [fb]
      (float-array (mapcat identity fb)))


