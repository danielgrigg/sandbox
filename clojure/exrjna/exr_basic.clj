(ns exr_basic)

(gen-interface
 :name jna.ExrBasic
 :extends [com.sun.jna.Library]
 :methods [[write_rgba [float []] Integer]])


