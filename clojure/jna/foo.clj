(ns foo)

(gen-interface
 :name jna.MyLibrary
 :extends [com.sun.jna.Library]
 :methods [[foo [] Integer]])

