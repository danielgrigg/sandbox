(ns countdownd.hello)                                                              

(def foo "foo")

(defn bar [x]
  (let [y (* x 2)]
    [x y]))

(defn qux
  ([x]
     (qux x (+ x 5)))
  ([x y]
     (* x y)))


(defn baz-destruct [{:keys [first-name surname]} n]
  (str "first-name " first-name " surname " surname " n " n))

(def o (js-obj "foo" 1 "bar" 2))
(def date-obj (js/Date))

(.log js/console "Hello world from js/console 2;\n"
      (baz-destruct {:first-name "Jane" :surname "Smith"} 32) ";\n"
      "qux(4): " (qux 4) ";\n"
      o)
(.log js/console "time " (. date-obj getDay))
(println "Hello world from REPL")

;;(js/alert "Hello from ClojureScript!")

