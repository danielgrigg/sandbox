(+ 2 7)
(- 11 5)
; reference last two evals
(* *1 *2)

; BT last exception
(.printStackTrace *e)

(load-file "temp.clj")

(defn date
  [person-1 person-2 & chaperones]
  (println person-1 "and" person-2 "went out with" (count chaperones) "chaperones."))

(date "Romeo" "Juliet" "Friar Lawrence" "Nurse")

					; anonymous
(use '[clojure.contrib.str-utils :only (re-split)])
(filter (fn [w] (> (count w) 2)) (re-split #"\W+" "A fine day"))

(filter #(> (count %) 2) (re-split #"\W+" "A fine day it is"))

; function scope
(defn indexable-words [text]
  (let [indexable-word? (fn [w] (> (count w) 2))]
    (filter indexable-word? (re-split #"\W+" text))))
(indexable-words "a fine day it is")

; closure
(defn make-greeter [greeting-prefix] (fn [username] (str greeting-prefix ", " username)))
(def aloha-greeting (make-greeter "Aloha"))
(aloha-greeting "world")

; let bindings
(defn square-corners [bottom left size]
  (let [top (+ bottom size)
	right (+ left size)]
    [[bottom left] [top left] [top right] [bottom right]]))
(square-corners 3 4 5)

					; destructuring
(defn greet-author-1 [author] (println "Hello," (:first-name author)))
(greet-author-1 {:last-name "Vinge" :first-name "Vernor"})
; via map
(defn greet-author-2 [{fname :first-name, lname :last-name}]
  (println "Hello," fname lname))
(greet-author-2 {:last-name "Vinge" :first-name "Vernor"})
; via array
(let [[x y] [1 2 3]] [x y])
; placeholders
(let [[_ _ z] [1 2 3]] z)
; binding destructuring and collection
(let [[x y :as coords] [1 2 3 4 5 6]]
  (str "x: " x ", y: " y ", coords: " coords))

(use '[clojure.contrib.str-utils :only (re-split str-join)])
(defn ellipsize [words]
  (let [[w1 w2 w3] (re-split #"\s+" words)]
    (str-join " " [w1 w2 w3 "..."])))
(ellipsize "the quick brown fox")

					; namespaces
(def foo 10)
(resolve 'foo)

(in-ns 'myapp)
(clojure.core/use 'clojure.core)

					; import java classes
(import '(java.io InputStream File))
				; load clojure lib
(require 'clojure.contrib.math)
(clojure.contrib.math/round 1.7)
					; map name to current namespace
(use 'clojure.contrib.math)
(round 1.7)
(use '[clojure.contrib.math :only (round)])

					; reload library
(use :reload '[clojure.contrib.math :only (round)])

					; prefer ns at the top
(ns myapp
  (:use (my.lib this that)))

(defn is-small? [number]
  (if (< number 100)
    "yes"
    (do
      (println "Saw a big number" number) "no" )))
(is-small? 200)
					; loop-recur
(loop [result [] x 5]
  (if (zero? x)
    result
    (recur (conj result x) (dec x))))

(defn countdown [result x]
  (if (zero? x)
    result
    (recur (conj result x) (dec x))))
(countdown [] 9)

; compose

(loop [result [] x 5] (if (zero? x) result (recur (conj result x) (dec x))))

((loop [c rhs] 
 (if (empty? rhs) 
  c 
  (recur ((inc c) (rest rhs))))) 

(loop [result 0 coll [1 2 3]] 
  (if (empty? coll)
    result
    (recur (inc result) (rest coll))))

