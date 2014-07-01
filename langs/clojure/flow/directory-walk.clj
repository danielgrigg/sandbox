(defn walk [dirpath pattern]
 (doseq [file (-> dirpath File. file-seq)]
  (if (re-matches pattern (.getName file))
   (println (.getPath file)))))
