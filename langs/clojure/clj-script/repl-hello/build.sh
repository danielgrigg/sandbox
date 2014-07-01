#!/bin/sh

cljsc hello.cljs > hello.js

# with optimizations
# cljsc hello.cljs '{:optimizations :advanced}' > hello.js
# see https://github.com/clojure/clojurescript/wiki/Quick-Start for details

