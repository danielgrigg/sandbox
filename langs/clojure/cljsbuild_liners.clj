:cljsbuild { 
            :builds 
            [{:source-path "src/cljs/project1" 
              :compiler {:output-to "resources/public/cljs/script1.js" 
                         :externs ["externs/jquery.js"] 
                         :optimizations :advanced 
                         :pretty-print true}} 
             {:source-path "src/cljs/project2" 
              :compiler {:output-to "resources/public/cljs/script2.js" 
                         :externs ["externs/jquery.js"] 
                         :optimizations :advanced 
                         :pretty-print true}}]} 

