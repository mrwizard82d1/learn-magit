(ns test-utils.word-source
  (:require [cljs.nodejs :as nodejs])
  (:use [clojure.string :only [split, split-lines]])
  )

(def fs (js/require "fs"))

(def sprintf (js/require "sprintf-js"))

(nodejs/enable-util-print!)

(defn read-file [name callback]
  (.readFile fs name "utf-8" callback))

(defn str->word-definitions 
  ([text]
   (->> (clojure.string/split-lines text)
        (partition 2))
   ))

(defn when-file-read [err data]
  (if err
    (println err)
    (let [definitions (str->word-definitions data)
          selected-definition (nth definitions (rand-int (count definitions)))]
      (println selected-definition))))

(defn dump-em [err data] 
  (if err
    (println "Error:" err)
    (println "Data:" data)))

(defn next-word []
  (read-file "latin_words.txt" when-file-read))

;; (defn -main [& args]
;;   (println "Hello world!")
;;   (read-file "latin_words.txt" when-file-read))

;; (set! *main-cli-fn* -main)

