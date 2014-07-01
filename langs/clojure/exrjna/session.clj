(jna-call :exr_basic "write_rgba" Integer 256 256)

; Remember to create 'classes' folder needed for (compile ) function.
; mkdir classes
; CLASSPATH=.:classes:jna.jar clj
(compile 'exr_basic)
(System/setProperty "jna.Library.path" "/Users/daniel/sandbox/clojure/jna")
(def exr-basic-lib (com.sun.jna.Native/loadLibrary "exr_basic" jna.ExrBasic))
(.write_rgba exr-basic-lib 512 256)

(import '(java.util Random))
(def rnd (Random. ))
(. rnd nextInt 10)
(. rnd (nextInt 10))

(defn noise-framebuffer [w h]
      (repeatedly (* w h) #(vector (rand) (rand) (rand) 1.0)))

(let [w 256 h 256] 
  (jna-call :exr_basic "write_rgba" Integer w h 
            (native-framebuffer (solid-framebuffer w h [1.0 0.0 0.0 1.0]))))

(let [w 512 h 512]
  (jna-call :exr_basic "write_rgba" Integer w h
            (native-framebuffer
              (noise-framebuffer w h))))
