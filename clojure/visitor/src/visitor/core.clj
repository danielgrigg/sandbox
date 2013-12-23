(ns visitor.core)

;; Short example of using multiple-dispatch to solve the visitor problem,
;; per Design Patterns.
(def visitor-types [:type-checking :code-generating])

(def node-1 {:node-type :variable-ref })
(def node-2 {:node-type :assignment })

; dispatch on node-type and visitor-type.
(defn node-visitor [node visitor-type]
  [(:node-type node) visitor-type])

(defmulti traverse-node node-visitor)
(defmethod traverse-node [:variable-ref ::type-checking] [node vt]
  (str "traversing " (:node-type node) " " (name vt)))

(defmethod traverse-node :default [node vt]
  (str "Unimplemented node traverse for " (str node) " with visitor " (name vt)))

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
