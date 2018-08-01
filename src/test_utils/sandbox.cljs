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

(defn rand-minimal-xy-monitor-treatment-distance []
  (let [minimum-euclidean-distance 946.9745
        x-component (* minimum-euclidean-distance (rand))
        y-component (Math/sqrt (- (* minimum-euclidean-distance minimum-euclidean-distance) (* x-component x-component)))]
    [x-component y-component]))

(defn rand-well-number []
  (inc (tuc/rand-digit)))

(defn rand-stage-number []
  (inc (tuc/rand-2)))

(defn typical-trajectory-md-length []
  (tuc/draw-normal 93.7 0.4))

(defn flat-trajectory [md x y z plot-angle]
  (let [next-fn (fn [[md0 x0 y0 z0]]
                  (let [step (typical-trajectory-md-length)
                        step-angle (tuc/draw-normal plot-angle (/ plot-angle 10))]
                    [(+ md0 step)
                     (+ x0 (* step (Math/cos (* (/ step-angle 180) Math.PI))))
                     (+ y0 (* step (Math/sin (* (/ step-angle 180) Math.PI))))
                     (tuc/draw-normal z (/ z 10))]))]
    (iterate next-fn [md x y z])))

(defn typical-treatment-times []
  (let [[year-0 month-0 day-0 hour-0 minute-0 second-0 :as start] (tuc/rand-timestamp 2016 2026)
        duration-hours (tuc/draw-normal 2.5 0.25)
        duration-hour (int duration-hours)
        duration-minute (int (* 60 (- duration-hours duration-hour)))
        duration-seconds (rand-nth (range 60))]
    [start
     [year-0 month-0 day-0 
      (rem (+ hour-0 duration-hour) 24)
      (rem (+ minute-0 duration-minute) 60)
      duration-seconds]]))


(defn typical-stage-separation []
  (tuc/draw-normal 45 3))

(defn stage-extents []
  (let [extent-0 (typical-stage-extent)
        next-fn (fn [[top-n-1 bottom-n-1] extent-0]
                   (let [bottom (- top-n-1 (typical-stage-separation))
                         top (- bottom (typical-stage-length))]
                     [top bottom]))]
    (iterate next-fn extent-0)))

