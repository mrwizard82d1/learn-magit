(ns test-utils.sandbox
  #?(:cljs (:require [test-utils.core :as tuc]
                     [test-utils.word-source :as ws])
     :clj (:require (test-utils [core :as tuc]
                                [word-source :as ws])
                    [clojure.string :as clj-str])))

;; Stuff I'm trying out

(defn random-uuid []
  (java.util.UUID/randomUUID))

(defn hex-uuid 
  ([] (hex-uuid (random-uuid)))
  ([uuid] (clj-str/join (map #(Long/toHexString %) [(.getMostSignificantBits uuid) (.getLeastSignificantBits uuid)]))))

(defn color []
  (map clj-str/join
       (map #(concat ["0x"] %)
            (take 4 (map clj-str/join (partition 2 (hex-uuid)))))))

(defn random-given-name-path []
  "Return a 'path' to a given name from [Wikipedia](https://en.wikipedia.org/wiki/List_of_most_popular_given_names)"
  (let [regions (concat (repeat 8 :americas)
                        (repeat 4 :europe)
                        (repeat 4 :asia)
                        [:africa :oceania])
        gender (concat (repeat 6 :male)
                       (repeat 4 :female))
        target {:americas {:male 20
                           :female 20}
                :europe {:male 50
                         :female 50}
                :asia {:male 32
                       :female 30}
                :africa {:male 9
                         :female 9}
                :oceania {:male 4
                          :female 4}}
        candidates [(rand-nth regions) (rand-nth gender)]]
    (flatten [candidates
              (inc (rand-int (get-in target candidates)))
              (inc (rand-int 10))])))
