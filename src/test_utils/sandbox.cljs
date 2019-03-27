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
  (inc (rem (tuc/rand-2) 50)))

(defn rand-isip []
  (Math/abs (tuc/draw-normal 5061 121)))

(defn rand-shmin []
  (Math/abs (tuc/draw-normal 2.29 0.05)))

(defn rand-cluster-count []
  (inc (rand-nth (range 2 7))))

(defn rand-cluster-number []
  (inc (rand-nth (range 7))))

(defn typical-trajectory-md-length []
  (tuc/draw-normal 93.7 0.4))

(defn flat-trajectory [md x y z plot-angle]
  (let [next-fn (fn [[md0 x0 y0 z0]]
                  (let [step (typical-trajectory-md-length)
                        step-angle (tuc/draw-normal plot-angle 14)]
                    [(+ md0 step)
                     (+ x0 (* step (Math/cos (* (/ step-angle 180) Math.PI))))
                     (+ y0 (* step (Math/sin (* (/ step-angle 180) Math.PI))))
                     (tuc/draw-normal z 4)]))]
    (iterate next-fn [md x y z])))

(defn typical-treatment-time-range 
  ([]
   (let [[year-0 month-0 day-0 hour-0 minute-0 second-0 :as start] (tuc/rand-timestamp 2016 2026)
        duration-hours (tuc/draw-normal 2.52 0.17)
        duration-hour (int duration-hours)
        duration-minute (int (* 60 (- duration-hours duration-hour)))
        duration-seconds (rand-nth (range 60))]
    [start
     [year-0 month-0 day-0 
      (rem (+ hour-0 duration-hour) 24)
      (rem (+ minute-0 duration-minute) 60)
      duration-seconds]]))
  ([[start-a start-mo start-d start-h start-min start-s]
    [stop-a stop-mo stop-d stop-h stop-min stop-s]]
   (let [start-duration (tuc/draw-normal 5.7 1.37)
         start-duration-h (int start-duration)
         start-duration-min (int (* 60 (- start-duration start-duration-h)))
         start-duration-s (rand-nth (range 60))
         start [start-a start-mo start-d 
                (rem (+ start-h start-duration-h) 24) 
                (rem (+ start-min start-duration-min) 60) 
                start-duration-h]
         stop-duration (tuc/draw-normal 2.52 0.17)
         stop-duration-h (int stop-duration)
         stop-duration-min (int (* 60 (- stop-duration stop-duration-h)))
         stop-duration-s (rand-nth (range 60))
         stop [start-a start-mo start-d
               (rem (+ (nth start 3) stop-duration-h) 24)
               (rem (+ (nth start 4) stop-duration-min) 60)
               stop-duration-s]]
     [start stop])))

(defn treatment-times 
  ([] (treatment-times (typical-treatment-time-range)))
  ([start-treatment-time-range]
   (let [[start-0 stop-0] start-treatment-time-range
         next-fn (fn [[start-n-1 stop-n-1]]
                   (typical-treatment-time-range start-n-1 stop-n-1))]
     (iterate next-fn [start-0 stop-0]))))

(defn typical-stage-separation []
  (tuc/draw-normal 45 3))

(defn stage-extents []
  (let [extent-0 (typical-stage-extent)
        next-fn (fn [[top-n-1 bottom-n-1] extent-0]
                  (let [bottom (- top-n-1 (typical-stage-separation))
                        top (- bottom (typical-stage-length))]
                    [top bottom]))]
    (iterate next-fn extent-0)))

(defn typical-interwell-distance [plot-angle]
  (let [euclidean-distance (tuc/draw-normal 874 37)
        x-distance (* euclidean-distance (Math/cos (* (/ (+ plot-angle 90) 180) Math.PI)))
        y-distance (* euclidean-distance (Math/sin (* (/ (+ plot-angle 90) 180) Math.PI)))]
    [x-distance y-distance]))

(defn typical-isip []
  (tuc/draw-normal 3102 497))

(defn typical-inclination []
  (tuc/draw-normal 92.2 2.4))

(defn typical-azimuth [plot-angle]
  (tuc/draw-normal plot-angle 1.24))

(defn make-line-generator [[m b]]
  (fn [t]
    (+ (* m t) b)))

(def p-mon-generator-parameters [[-1.8590e-02   2.8583e+02]
                                 [-3.0727e-03   6.0814e+01]
                                 [-4.6937e-03   1.0586e+02]
                                 [7.9858e-03   9.6910e+00]
                                 [7.9035e-02  -1.7834e+02]
                                 [-7.5163e-03   1.6379e+02]
                                 [-6.5209e-03   1.2149e+02]
                                 [-7.1622e-03   1.1633e+02]
                                 [-6.3279e-03   1.1242e+02]
                                 [3.4088e-02  -4.6007e+01]
                                 [-1.6031e-02   2.8456e+02]
                                 [-3.3784e-03   1.0165e+02]
                                 [2.7285e-02  -4.6048e+01]
                                 [1.0749e-01  -1.2872e+02]
                                 [-5.3974e-03   1.3903e+02]
                                 [-3.6674e-03   7.7673e+01]
                                 [-3.7600e-03   7.0792e+01]
                                 [-2.6080e-03   4.3603e+01]
                                 [-1.6967e-02   2.7267e+02]
                                 [-1.0160e-02   1.6014e+02]
                                 [-6.9151e-03   1.1013e+02]
                                 [-2.8395e-03   5.2684e+01]
                                 [1.4582e-04   1.4241e+01]
                                 [1.4412e-03   1.1006e+01]
                                 [2.0431e-02  -2.4441e+01]
                                 [6.5569e-02  -1.0583e+02]
                                 [-1.4590e-04   4.0460e+01]
                                 [-4.1713e-03   8.6142e+01]
                                 [-2.7311e-03   5.5830e+01]
                                 [7.8723e-02  -3.5040e+02]
                                 [-1.0843e-02   1.6752e+02]
                                 [-3.3414e-03   5.9796e+01]
                                 [-2.7253e-03   4.5386e+01]
                                 [-1.6702e-03   2.4405e+01]])

(defn rand-stage-time-point-seconds []
  (let [time-seconds (* 3600 (tuc/draw-normal 2.52 0.17))]
    (* time-seconds (rand))))

(defn make-pressure-generator [generator-parameters]
  (make-line-generator (rand-nth generator-parameters)))

(let [generator (make-pressure-generator p-mon-generator-parameters)]
  (defn rand-p-mon
    ([] (rand-p-mon (rand-stage-time-point-seconds)))
    ([at-time] (generator at-time))))

(defn p-mon-seq []
  (let [start-at (rand-stage-time-point-seconds)
        at-times (iterate (fn [t] (+ t 30)) start-at)]
    (map #(rand-p-mon %) at-times)))

(def delta-p-mon-generator-parameters [[-1.6031e-02 2.8456e+02]
                                       [-3.3784e-03 1.0165e+02]
                                       [2.7285e-02 -4.6048e+01]
                                       [1.0749e-01 -1.2872e+02]
                                       [-5.3974e-03 1.3903e+02]
                                       [-3.6674e-03 7.7673e+01]
                                       [-3.7600e-03 7.0792e+01]
                                       [-2.6080e-03 4.3603e+01]
                                       [-1.6967e-02 2.7267e+02]
                                       [-1.0160e-02 1.6014e+02]
                                       [-6.9151e-03 1.1013e+02]
                                       [-2.8395e-03 5.2684e+01]
                                       [1.4582e-04 1.4241e+01]
                                       [1.4412e-03 1.1006e+01]
                                       [-1.8590e-02 2.8583e+02]
                                       [2.0431e-02 -2.4441e+01]
                                       [-3.0727e-03 6.0814e+01]
                                       [6.5569e-02 -1.0583e+02]
                                       [-4.6937e-03 1.0586e+02]
                                       [-1.4590e-04 4.0460e+01]
                                       [7.9858e-03 9.6910e+00]
                                       [-4.1713e-03 8.6142e+01]
                                       [7.9035e-02 -1.7834e+02]
                                       [-2.7311e-03 5.5830e+01]
                                       [-7.5163e-03 1.6379e+02]
                                       [7.8723e-02 -3.5040e+02]
                                       [-6.5209e-03 1.2149e+02]
                                       [-1.0843e-02 1.6752e+02]
                                       [-7.1622e-03 1.1633e+02]
                                       [-3.3414e-03 5.9796e+01]
                                       [-6.3279e-03 1.1242e+02]
                                       [-2.7253e-03 4.5386e+01]
                                       [3.4088e-02 -4.6007e+01]
                                       [-1.6702e-03 2.4405e+01]])

(let [generator (make-pressure-generator delta-p-mon-generator-parameters)]
  (defn rand-delta-p-mon
    ([] (rand-delta-p-mon (rand-stage-time-point-seconds)))
    ([at-time] (generator at-time))))

(defn delta-p-mon-seq []
  (let [start-at (rand-stage-time-point-seconds)
        at-times (iterate (fn [t] (+ t 30)) start-at)]
    (map #(rand-delta-p-mon %) at-times)))

(def delta-p-generator-parameters [[0.0043045 -2.8756]
                                   [0.0060165 -6.048]
                                   [0.033074 -72.432]
                                   [0.11563 -179.6]
                                   [0.0079444 27.84]
                                   [0.0025804 -0.37932]
                                   [0.0012415 -1.4843]
                                   [-0.00012783 0.29748]
                                   [0.0005664 1.1103]
                                   [0.0011247 -0.86387]
                                   [0.00079915 -1.3032]
                                   [0.0013406 -2.1837]
                                   [0.0026866 -3.2869]
                                   [0.0022875 9.4414]
                                   [0.00080767 1.6558]
                                   [0.020873 -46.359]
                                   [0.0048643 -8.7268]
                                   [0.066701 -112.77]
                                   [0.003416 -2.5972]
                                   [0.0057154 -28.564]
                                   [0.011914 -17.788]
                                   [0.0034755 -2.7982]
                                   [0.083909 -202.02]
                                   [0.0017028 -1.1252]
                                   [0.0097729 6.3774]
                                   [0.081377 -399.33]
                                   [0.0044428 -7.1937]
                                   [0.00074879 1.59]
                                   [0.0039333 -8.0394]
                                   [0.0027725 -5.7062]
                                   [0.0026997 -4.9229]
                                   [0.002153 -5.7688]
                                   [0.043904 -190.32]
                                   [0.0040147 -3.9782]])

(let [generator (make-pressure-generator delta-p-generator-parameters)]
  (defn rand-delta-p
    ([] (rand-delta-p (rand-stage-time-point-seconds)))
    ([at-time] (generator at-time))))

(defn delta-p-seq []
  (let [start-at (rand-stage-time-point-seconds)
        at-times (iterate (fn [t] (+ t 30)) start-at)]
    (map #(rand-delta-p %) at-times)))

(defn rand-fhl []
  (tuc/draw-normal 773 106))

(defn rand-fracture-height []
  (tuc/draw-normal 246.8 63.0))

(defn rand-ratio []
  (tuc/draw-normal 3.09 0.42))
