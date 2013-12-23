(ns dan)

(gen-interface
 :name jna.CLibrary
 :extends [com.sun.jna.Library]
 :methods [[printf [String] void]])

