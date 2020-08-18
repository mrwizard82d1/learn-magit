(ns test-utils.word-source
   (:require [clojure.string :as clj-str]))

#?(:cljs (def fs (js/require "fs")))

(defn read-text-file [pathname]
  #?(:cljs (.readFileSync fs pathname "utf-8"))
  #?(:clj (slurp pathname)))

(defn empty-string? [to-test]
  "Determine if to-test is an empty string."
  (and (string? to-test)
       (== #?(:cljs (.-length to-test))
           #?(:clj (.length to-test))
           0)))

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

(defn rand-words
  "Return a sequence of `n` (defaults to 3) random words."
  ([] (rand-words 3))
  ([n] (repeatedly n rand-word)))

(defn noun-mnemonic [declension]
  (println
    (case declension
      1 (clj-str/join "\n" ["Maria, queen of reggae, gave Fannie Mae some jam for her bananā.",
                            "Fannie Mae, fond of ā rum, gave the Israelīs some bananās from the delīs."])
      2 (clj-str/join "\n" ["Gus and Peter, friends of Luigī, gave Mariō some gum for his burritō.",
                            "He and Ī, kings of the quōrum, gave the Israelīs some burritōs from the delīs."])
      3 (clj-str/join "\n" ["The Black Hole Gang*, friends of Beavis, gave Bambī a gem from Chile.",
                            "The Apachēs, masters of the drum, gave the minibus some tamalēs from the omnibus."]))))

(defn grammar-gender [] (rand-nth [:m :f :n]))

(defn grammar-number [] (rand-nth [:s :pl]))

(defn grammar-case [] (rand-nth [:nom :gen :dat :acc :abl :voc]))

(defn grammar-person [] (rand-nth [:1st :2nd :3rd]))

(defn grammar-mood [] (rand-nth [:indicative :subjunctive :imperative :infinitive]))

(defn grammar-tense [] (rand-nth [:present :imperfect :future :perfect :pluperfect :future-perfect]))

(defn declension [] [(grammar-number) (grammar-case)])

(defn conjugation [] [(grammar-person) (grammar-mood) (grammar-number) (grammar-tense)])
