(ns exchange-rate.core
  (:gen-class)
  (:require [clojure.xml :as xml]))

(defn currencies []
  (map (fn [{{c :currency r :rate} :attrs}] (str c " ~ " r))
       (xml/content
        (first
         (xml/content
          (first
           (filter #(= (xml/tag %) :Cube)
                   (xml/content
                    (xml/parse "http://www.ecb.europa.eu/stats/eurofxref/eurofxref-daily.xml")))))))))

(defn -main [& args]
  (println "Currencies:\n" (apply str (interpose "\n" (doall (currencies))))))
