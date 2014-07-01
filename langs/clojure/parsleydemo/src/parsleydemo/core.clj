(ns parsleydemo.core
  (:gen-class)
  (:require [clojure.xml :as xml])
  (:require [net.cgrand.parsley :as p]))

(def box_p
  (str "mtllib box_p.mtl\n"
       "g default\n"
       "v -3.000000 -1.000000 2.000000\n"
       "v 3.000000 -1.000000 2.000000\n"
       "v -3.000000 1.000000 2.000000\n"
       "v 3.000000 1.000000 2.000000\n"
       "v -3.000000 1.000000 -2.000000\n"
       "v 3.000000 1.000000 -2.000000\n"
       "v -3.000000 -1.000000 -2.000000\n"
       "v 3.000000 -1.000000 -2.000000\n"
       "s off\n"
       "g pCube1\n"
       "usemtl initialShadingGroup\n"
       "f 1 2 4 3\n"
       "f 3 4 6 5\n"
       "f 5 6 8 7\n"
       "f 7 8 2 1\n"
       "f 2 8 6 4\n"
       "f 7 1 3 5\n"))

(def simpleobj
  (str "g default\n"
       "v -3.000000 -1.000000 2.000000\n"
       "v 3.000000 -1.000000 2.000000\n"       
       "f 1 2 4 3\n"
       "f 7 1 3 5\n"))


(def objp (p/parser {:main [#{:mtllib :g :v :f :usemtl :s}:*]
                     :space :ws?}
                   :ws #"\s+"
                   :mtllib ["mtllib" :name]
                   :g ["g" :name]
                   :v ["v" :float :float :float]
                   :f ["f" :int :int :int :int]
                   :s #{"off" "on"}
                   :usemtl ["usemtl" :name]
                   :float #"\d+"
                   :int #"\d+"
                   :name #"\S+"))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
