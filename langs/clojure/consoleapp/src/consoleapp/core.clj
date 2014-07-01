(ns consoleapp.core
    (:gen-class))

(defn -main [& args]
      (println 
       (apply str (interpose " " 
                   (map (memfn toLowerCase) args)))))


