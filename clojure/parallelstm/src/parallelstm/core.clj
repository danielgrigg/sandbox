(ns parallelstm.core)

(import '(java.util.concurrent Executors))
 
(defn test-stm [nitems nthreads niters]
  (let [refs  (map ref (repeat nitems 0))
        pool  (Executors/newFixedThreadPool nthreads)
        tasks (map (fn [t]
                      (fn []
                        (dotimes [n niters]
                          (dosync
                            (doseq [r refs]
                              (alter r + 1 t))))))
                   (range nthreads))]
    (doseq [future (.invokeAll pool tasks)]
      (.get future))
    (.shutdown pool)
    (map deref refs)))

(defn test-stm2 [nthreads]
  (let [pool (Executors/newFixedThreadPool nthreads)
        tasks (map 
                (fn [t]
                  (fn []
                    (Thread/sleep 1000)
                    (println t " done.")))
                (range nthreads))]
    (doseq [future (.invokeAll pool tasks)]
      (.get future))
    (.shutdown pool)))

;; (test-stm 10 10 10000)

