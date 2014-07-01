(ns fileio.core
  (:gen-class))

(use 'clojure.java.io)

(defn test-read-file []
  (slurp "/tmp/test.txt"))

(defn test-write-file []
  (with-open [w (writer "/tmp/test.txt")]
    (.write w "the quick\nbrown fox\njumped over\nthe lazy dog")))

(defn test-read-file-lines []
  (with-open [r (reader "/tmp/test.txt")]
    (doseq [l (line-seq r)]
      (println l))))

(defn -main
  "I don't do a whole lot."
  []
  (do
    (test-write-file)
    (println (test-read-file))
    (test-read-file-lines)))

