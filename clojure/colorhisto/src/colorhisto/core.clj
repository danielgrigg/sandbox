(ns colorhisto.core
  (:gen-class))

(import '(java.io FileInputStream))
(import '(javax.imageio ImageIO))

(defn histogram [nbins ls]
  (reduce (fn [s [k v]] (assoc s k v ))
          (vec (repeat nbins 0)) (frequencies ls)))

(defn random-data [n max-val]
  (repeatedly n #(rand-int max-val)))

(defn int32-to-rgba [c]
  [(bit-shift-right (bit-and 0xFF000000 c) 24)
   (bit-shift-right (bit-and 0x00FF0000 c) 16)
   (bit-shift-right (bit-and 0x0000FF00 c) 8)
   (bit-and 0x000000FF c)])

(defn image-pixels [fname]
  (with-open [r (java.io.FileInputStream. fname)]
    (let [image (javax.imageio.ImageIO/read r)
          w (.getWidth image)
          h (.getHeight image)]
      (mapcat int32-to-rgba (for [y (range h) x (range w)] (.getRGB image x y)) ))))

(defn channel [n ps]
  (take-nth 4 (drop n ps)))

(defn image-histogram [fname n]
  (let [ps (image-pixels fname)]
    (map (partial histogram 256) (for [c [1 2 3]] (channel c ps)))))
  
(defn -main
  "I don't do a whole lot."
  [& args]
  (let [[fname n-str] (take 2 args)]
    (do
      (println (apply str (image-histogram fname (read-string n-str)))))))
