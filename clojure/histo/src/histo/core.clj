(ns histo.core
  (:gen-class)
  (:use [incanter core io charts stats]))

(defn -main [& args]
  (view (histogram (sample-normal 1000))))

