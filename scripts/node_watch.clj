(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'test-utils.core
   :output-to "out/test_utils.js"
   :output-dir "out"})
