(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'test-utils-cljs.core
   :output-to "out/test_utils_cljs.js"
   :output-dir "out"})
