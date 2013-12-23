(defproject hello_cljs "0.1.0"
  :description "FIXME: write description"
  :source-paths ["src-clj"]
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [compojure "1.1.0"]
                 [hiccup "1.0.0"]]
  :plugins [[lein-ring "0.7.1"]
            [lein-cljsbuild "0.2.1"]]
  :cljsbuild {
    :builds [{:source-path "src-cljs"
              :compiler {:output-to "resources/public/js/main.js"
                         :optimizations :whitespace
                         :pretty-print true}}]}            
  :ring {:handler hello_cljs.routes/app})

