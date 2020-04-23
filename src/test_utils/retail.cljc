(ns test-utils.retail
  #?(:cljs (:require [test-utils.core :as tuc]
                     [test-utils.word-source :as ws])
     :clj (:require (test-utils [core :as tuc]
                                [word-source :as ws]))))

(defn name-abbreviation-pair []
  (repeatedly 3 ws/rand-word))

(defn person-name []
  [(ws/rand-word) (if (= 0 (rem (rand-int 300) 3)) (tuc/rand-alpha) "") (ws/rand-word)])

(defn date []
  (str (tuc/rand-range 1999 2023) "-"
       (tuc/two-digit-zero-pad (tuc/rand-range 1 13)) "-"
       (tuc/two-digit-zero-pad (tuc/rand-range 1 32))))

(defn wallclock []
  (str (tuc/two-digit-zero-pad (tuc/rand-range 0 25)) ":"
       (tuc/two-digit-zero-pad (tuc/rand-range 0 60)) ":"
       (tuc/two-digit-zero-pad (tuc/rand-range 0 60))))

(defn date-time []
  (str (date) "T" (wallclock)))

(defn price []
  [(tuc/rand-2) (tuc/rand-2)])

(defn tax []
  [(tuc/rand-digit) (tuc/rand-2)])
