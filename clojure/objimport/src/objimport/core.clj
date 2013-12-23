(ns objimport.core
  (:gen-class)
  (:use [clojure.string :only (blank? split-lines)])
  (:import (java.io FileReader BufferedReader)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defmacro bench [n & exprs]
  `(time
   (dotimes [~'_ ~n]
     (do ~@exprs))))

(defn obj-path [prefix name]
  (str prefix name ".obj"))

(defn parse-float [^String x] (Float/parseFloat x))
(defn parse-int [^String x] (Integer/parseInt x))

(defn parse-vertex [^String line]
  (apply vector-of :float
       (map parse-float (next (re-seq #"\S+" line)))))
  
(defn parse-face [^String line]
  (apply vector-of :int
         (map parse-int (re-seq #"\d+" line))))

(defn obj-lines [^String path] 
    (remove blank? (split-lines (slurp path))))

(defn parse-line [m ^String line]
  (let [[key f] (get (all-parsers) (first line) [:ignored identity])]
    (update-in m [key] conj (f line))))   
              
(defn model 
  ([]
     {:vertices [] :faces [] :ignored []})
([prefix name]
   (reduce parse-line (model) (obj-lines (obj-path prefix name)))))

  

(defn model-flatten [{:keys [vertices faces] :as m}]
  (let [de-index (partial nth vertices)
        faces' (for [f faces 
                     :let [f' (map dec f)]]
                 (map de-index f'))]
    (assoc-in m [:faces] faces')))


(defn test-path []
  (str (get (System/getenv) "HOME") "/content/models/test/"))

(defn test-model [name]
  (model (test-path) name))

(defn -main [& args]
  (bench 1 (test-model (first args))))

(defn model-buffered [prefix name]
  (with-open [rdr (java.io.BufferedReader. 
                   (java.io.FileReader. (obj-path prefix name)))]
    (reduce parse-line nil (line-seq rdr))))
