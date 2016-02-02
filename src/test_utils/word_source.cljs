(ns test-utils.word-source
   (:require [cljs.nodejs :as nodejs]
             [clojure.string :as clj-str]))

(def fs (js/require "fs"))

(defn read-text-file [pathname]
  (.readFileSync fs pathname "utf-8"))

(defn empty-string? [to-test]
  "Determine if to-test is an empty string."
  (and (string? to-test)
       (== (.-length to-test) 0)))

(defn parse-lines [text]
  (clj-str/split text #"\n"))

(defn empty-string-sequence? [to-test]
  "Determine if to-test is a sequence containing only an empty string."
  (and (= 1 (count to-test))
       (empty-string? (first to-test))))

(defn parse-definitions [lines]
  (->> lines
       (drop 2)
       (map #(.trim %))
       (partition-by empty-string?)
       (remove empty-string-sequence?)))

(defn read-latin-words-from [pathname]
  (let [definitions (-> pathname
                        (read-text-file)
                        (parse-lines)
                        (parse-definitions))]
    (map #(str (first %) (second %)) definitions)))

(def read-latin-words (memoize read-latin-words-from))

(defn rand-word
  ([] (rand-word (read-latin-words "latin-words.md") rand-nth))
  ([words rand-f] (rand-f words)))

