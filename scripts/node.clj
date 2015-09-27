(require 'cljs.build.api)

(println "Building for node...")

(cljs.build.api/build "src"
                      {:main 'test-utils.core
                       :output-to "out/test_utils.js"
                       :output-dir "out"
                       :target :nodejs
                       :verbose true})
