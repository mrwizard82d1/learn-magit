(ns test-utils.sandbox
  (:require [test-utils.core :as tuc] 
            [test-utils.word-source :as ws]))

;; Stuff I'm trying out

(defn name-abbreviation-pair []
  (repeatedly 3 ws/rand-word))

(defn person-name []
  [(ws/rand-word) (if (= 0 (rem (rand-int 300) 3)) (tuc/rand-alpha) "") (ws/rand-word)])

(defn house-number []
  (tuc/rand-4))

(defn street-name []
  (repeatedly 2 ws/rand-word))

(defn zip-code []
  (tuc/rand-5))

(defn phone-number []
  (str "555-" (tuc/rand-3) "-" (tuc/rand-4)))

(defn date []
  (str (tuc/rand-range 1999 2023) "-"
       (tuc/rand-range 1 13) "-"
       (tuc/rand-range 1 32) " "))

