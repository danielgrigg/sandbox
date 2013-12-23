cljsc hello.cljs '{:optimizations :advanced}' > hello.js
#cljsc hello.cljs '{:optimizations :simple :pretty-print true}' > hello_opt.js
open hello_opt.html


