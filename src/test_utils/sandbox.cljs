(ns test-utils.sandbox
  (:require [test-utils.core :as tuc]
            [test-utils.word-source :as ws]))

;; Stuff I'm trying out

(defn name-abbreviation-pair []
  (repeatedly 3 ws/rand-word))

(defn person-name []
  [(ws/rand-word) (if (= 0 (rem (rand-int 300) 3)) (tuc/rand-alpha) "") (ws/rand-word)])

(defn two-digit-zero-pad [number]
  (if (>= number 10)
    number
    (str "0" number)))

(defn date []
  (str (tuc/rand-range 1999 2023) "-"
       (two-digit-zero-pad (tuc/rand-range 1 13)) "-"
       (two-digit-zero-pad (tuc/rand-range 1 32))))

(defn wallclock []
  (str (two-digit-zero-pad (tuc/rand-range 0 25)) ":"
       (two-digit-zero-pad (tuc/rand-range 0 60)) ":"
       (two-digit-zero-pad (tuc/rand-range 0 60))))

(defn date-time []
  (str (date) "T" (wallclock)))

(defn price []
  [(tuc/rand-2) (tuc/rand-2)])

(defn tax []
  [(tuc/rand-digit) (tuc/rand-2)])

(defn typical-vertical-depth []
  (tuc/draw-normal 8000 1216))

(defn typical-toe-measured-depth []
  (tuc/draw-normal 18000 1500))

(defn typical-stage-top []
  (tuc/draw-normal 13750 2150))

(defn typical-stage-length []
  (tuc/draw-normal 152 17))

(defn typical-stage-extent
  ([]
   (typical-stage-extent typical-stage-top))
  ([top-f]
   (let [top (top-f)
         length (typical-stage-length)]
     (vector top (+ top length)))))

(defn rand-easting []
  (let [minimum-easting 167000
        maximum-easting 833000]
    (tuc/rand-range minimum-easting (inc maximum-easting))))

(defn rand-northing []
  (let [maximum-northing 1e7]
    (tuc/rand-range maximum-northing)))
