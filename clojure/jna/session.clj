(def glibc (Native/loadLibrary "c" jna.CLibrary))
(.printf glibc "Hello, World.. \n")

(defmacro jna-call [lib func ret & args] 
  `(let [library#  (name ~lib)
           function# (com.sun.jna.Function/getFunction library# ~func)] 
           (.invoke function# ~ret (to-array [~@args]))))

(jna-call :c "printf" Integer "kjhkjh")

;;Some POSIX Calls
(jna-call :c "mkdir" Integer "/tmp/jnatesttemp" 07777)
(jna-call :c "rename" Integer "/tmp/jnatesttemp" "/tmp/jnatesttempas")
(jna-call :c "rmdir" Integer "/tmp/jnatesttempas")


(System/setProperty "jna.Library.path" "/Users/daniel/sandbox/clojure/jna")
(def foolib (com.sun.jna.Native/loadLibrary "my_lib" jna.MyLibrary))
(.foo foolib)


