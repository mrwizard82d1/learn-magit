(ns test-utils.fracture-diagnostics
  #?(:cljs (:require [test-utils.core :as tuc])
     :clj (:require (test-utils [core :as tuc]))))

(def physical-quantities [:densty
                          :energy
                          :force
                          :length
                          :mass
                          :power
                          :pressure
                          :proppant-concentration
                          :slurry-rate
                          :temperature
                          :volume])

(defn rand-physical-quantity []
  (rand-nth physical-quantities))

(defn rand-unit [physical-quantity]
  (let [quantity-units-map {:density                [:lb-per-cu-ft :kg-per-m3]
                            :energy                 [:ft-lb :J]
                            :force                  [:lbf :N]
                            :length                 [:ft :m]
                            :mass                   [:lb :kg]
                            :power                  [:hp :W]
                            :pressure               [:psi :kPa]
                            :proppant-concentration [:lb-per-gal :lb/gal :kg-per-m3 :kg/m3]
                            :slurry-rate            [:bbl-per-min :bpm :m3-per-min :m3/min]
                            :volume                 [:bbl :m3]
                            :temperature            [:F :C]}]
    (fn [] (rand-nth (get quantity-units-map physical-quantity)))))

(def rand-density-unit (rand-unit :density))
(def rand-energy-unit (rand-unit :energy))
(def rand-force-unit (rand-unit :force))
(def rand-length-unit (rand-unit :length))
(def rand-mass-unit (rand-unit :mass))
(def rand-power-unit (rand-unit :power))
(def rand-pressure-unit (rand-unit :pressure))
(def rand-proppant-concentration-unit (rand-unit :proppant-concentration))
(def rand-slurry-rate-unit (rand-unit :slurry-rate))
(def rand-temperature-unit (rand-unit :temperature))
(def rand-volume-unit (rand-unit :volume))

(defn convert-units-f [from to]
  (let [factors   {[:lb-per-cu-ft :kg-per-m3] 16.0185
                   [:ft-lb :J]                1.35582
                   [:lbf :N]                  4.44822
                   [:m :ft]                   3.28084
                   [:kg :lb]                  2.20462262185
                   [:hp :W]                   745.69987158227022
                   [:psi :kPa]                6.894757293168361
                   [:lb-per-gal :kg-per-m3]   119.826
                   ;; slurry rate conversion not needed (use [:bbl :m3])
                   [:m3 :bbl]                 6.28981077}
        converter {[:lb-per-cu-ft :kg-per-m3] #(* % (factors [:lb-per-cu-ft :kg-per-m3]))
                   [:kg-per-m3 :lb-per-cu-ft] #(/ % (factors [:lb-per-cu-ft :kg-per-m3]))
                   [:ft-lb :J]                #(* % (factors [:ft-lb :J]))
                   [:J :ft-lb]                #(/ % (factors [:ft-lb :J]))
                   [:lbf :W]                  #(* % (factors [:lbf :W]))
                   [:W :lbf]                  #(/ % (factors [:lbf :W]))
                   [:ft :m]                   #(/ % (factors [:m :ft]))
                   [:m :ft]                   #(* % (factors [:m :ft]))
                   [:kg :lb]                  #(* % (factors [:kg :lb]))
                   [:lb :kg]                  #(/ % (factors [:kg :lb]))
                   [:hp :W]                   #(* % (factors [:hp :W]))
                   [:W :hp]                   #(/ % (factors [:hp :W]))
                   [:psi :kPa]                #(* % (factors [:psi :kPa]))
                   [:kPa :psi]                #(/ % (factors [:psi :kPa]))
                   [:lb-per-gal :kg-per-m3]   #(* % (factors [:lb-per-gal :kg-per-m3]))
                   [:kg-per-m3 :lb-per-gal]   #(/ % (factors [:lb-per-gal :kg-per-m3]))
                   [:m3-per-min :bbl-per-min] #(* % (factors [:m3 :bbl]))
                   [:bbl-per-min :m3-per-min] #(/ % (factors [:lb-per-gal :kg-per-m3]))
                   [:F :C]                    #(/ (- % 32) 1.8)
                   [:C :F]                    #(+ (* % 1.8) 32)
                   [:bbl :m3]                 #(/ % (factors [:m3 :bbl]))
                   [:m3 :bbl]                 #(* % (factors [:m3 :bbl]))}]
    (converter [from to])))

(defn typical-vertical-depth []
  (tuc/draw-normal 8000 1216))

(defn typical-measured-depth []
  (+ (tuc/draw-normal 8000 1216)
     (* 5280 (rand))))

(defn typical-toe-measured-depth []
  (tuc/draw-normal 18000 1500))

(defn typical-stage-top []
  (tuc/draw-normal 13750 2150))

(defn typical-stage-length
  ([] (typical-stage-length (rand-length-unit)))
  ([length-unit]
   (condp = length-unit
     :ft [(tuc/draw-normal 152 17) :ft]
     :m [((convert-units-f :ft :m) (nth (typical-stage-length :ft) 0)) :m])))

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
        y-component (Math/sqrt (- (* minimum-euclidean-distance minimum-euclidean-distance)
                                  (* x-component x-component)))]
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
                     (+ x0 (* step (Math/cos (* (/ step-angle 180) #?(:cljs Math.PI)))))
                     (+ y0 (* step (Math/sin (* (/ step-angle 180) #?(:cljs Math.PI)))))
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

(defn typical-inter-well-distance [plot-angle]
  (let [euclidean-distance (tuc/draw-normal 874 37)
        x-distance (* euclidean-distance (Math/cos (* (/ (+ plot-angle 90) 180) #?(:cljs Math.PI))))
        y-distance (* euclidean-distance (Math/sin (* (/ (+ plot-angle 90) 180) #?(:cljs Math.PI))))]
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

(defn value-from-typical-range [typical-min typical-max]
  "Generate a normally-distributed, random value with the range typical-min to typical-max, inclusive."
  {:pre (< typical-min typical-max)}
  (let [mean (/ (+ typical-min typical-max) 2)
          ;; Typical range (assumed normal) is within 3-sigma of the mean
        sigma (/ mean 6)]
    (tuc/draw-normal mean sigma)))

(defn typical-monitor-pressure
  ([units]
   (cond (= units :psi)
         (first (p-mon-seq))
         (= units :kPa)
         ((convert-units-f :psi :kPa) (typical-monitor-pressure :psi))
         (= units :MPa)
         ((convert-units-f :kPa :MPa) (typical-monitor-pressure :kPa))))
  ([] (let [unit (rand-pressure-unit)]
        [unit (typical-monitor-pressure unit)])))

(defn typical-monitor-temperature
  ([] (let [units (rand-nth [:C :F])]
        (typical-monitor-temperature units)))
  ([units] (cond (= units :C)
                 (value-from-typical-range 50 80)
                 (= units :F)
                 ((convert-units-f :C :F) (typical-monitor-temperature :C)))))

(defn typical-surface-treating-pressure
  ([]
   (let [units (rand-pressure-unit)]
     [units (typical-surface-treating-pressure units)]))
  ([units]
   (cond (= units :psi)
         (value-from-typical-range 5000 9000)
         (= units :kPa)
         ((convert-units-f :psi :kPa) (typical-monitor-pressure :psi))
         (= units :MPa)
         ((convert-units-f :kPa :MPa) (/ (typical-monitor-pressure :kPa) 1000)))))

(defn surface-treating-pressure-seq [arg]
  (cond (int? arg)
        (let [unit (rand-pressure-unit)]
          [unit (take arg (surface-treating-pressure-seq unit))])
        (keyword? arg)
        (repeatedly (fn [] (typical-surface-treating-pressure arg)))))

(defn typical-slurry-rate
  ([]
   (let [unit (rand-slurry-rate-unit)]
     (typical-slurry-rate unit)))
  ([unit]
   (let [typical-value (cond (or (= unit :bbl-per-min)
                                 (= unit :bpm))
                             (value-from-typical-range 75 100)
                             (or (= unit :m3-per-min)
                                 (= unit :m3/min))
                             (value-from-typical-range 11.92 15.9))]
     [typical-value unit])))

(defn slurry-rate-seq [arg]
  (cond (int? arg)
        (let [unit (rand-slurry-rate-unit)]
          [unit (take arg (slurry-rate-seq unit))])
        (keyword? arg)
        (repeatedly (fn [] (typical-slurry-rate arg)))))

(defn typical-proppant-concentration
  ([]
   (let [unit (rand-proppant-concentration-unit)]
     (typical-proppant-concentration unit)))
  ([unit]
   (let [typical-value (cond (or (= unit :lb-per-gal)
                                 (= unit :lb/gal))
                             (value-from-typical-range 0.2 10)
                             (or (= unit :kg-per-m3)
                                 (= unit :kg/m3))
                             (let [conversion (/ 0.453592 0.00378541)]
                                (value-from-typical-range (* 0.2 conversion) (* 10 conversion))))]
     [typical-value unit])))AbstractMethodError

(defn proppant-concentration-seq [arg]
  (cond (int? arg)
        (let [unit (rand-proppant-concentration-unit)]
          [unit (take arg (proppant-concentration-seq unit))])
        (keyword? arg)
        (repeatedly (fn [] (typical-proppant-concentration arg)))))

(defn rand-uwi []
  (apply str
         (interpose "-"
                    (map str [(tuc/two-digit-zero-pad (tuc/rand-2))
                              (tuc/rand-3)
                              (tuc/rand-5)
                              (tuc/two-digit-zero-pad (tuc/rand-2))
                              (tuc/two-digit-zero-pad (tuc/rand-2))]))))

(defn rand-sensor-id []
  (let [sensor-indicators ["mp" "mt" "tp", "pc", "ir"]
        indicator (rand-nth sensor-indicators)
        discriminator (tuc/rand-3)
        separator (if (rand-nth [true false]) "-" "")]
    (str indicator separator discriminator)))

(defn typical-fracture-size [units]
  (cond (= units :ft)
        [(value-from-typical-range 500 1000) (value-from-typical-range 100 150)]
        (= units :m)
        (map (convert-units-f :ft :m) (typical-fracture-size :ft))))

(defn typical-fracture-azimuth []
  (tuc/draw-normal 90 30))

(defn typical-fracture-geometry
  ([unit] [(map #(vector unit %) (typical-fracture-size unit)) (typical-fracture-azimuth)])
  ([] (typical-fracture-geometry (rand-length-unit))))

(defn typical-subsurface-location
  ([] (typical-subsurface-location (rand-length-unit)))
  ([length-unit]
   (let [typical-location-in-ft [(rand-easting) (rand-northing) (typical-vertical-depth)]]
     (if (= length-unit :ft)
       [:ft typical-location-in-ft]
       [:m (vec (map (convert-units-f :ft :m) typical-location-in-ft))]))))

(defn rand-well-reference-frame-xy []
  (rand-nth [:project :well-head :absolute-state-plane]))

(defn rand-depth-datum []
  (rand-nth [:ground-level :kelly-bushing :sea-level]))

(defn typical-kelly-bushing-elevation []
  (tuc/draw-normal 30.48 (/ 1.00 3.00)))

(defn typical-pumped-volume
  ([] (let [volume-unit (rand-volume-unit)]
        [(typical-pumped-volume volume-unit) volume-unit]))
  ([volume-unit]
   (condp = volume-unit
     :bbl (tuc/draw-normal 6986.70 1992.30)
     :m3 (tuc/draw-normal 813.47 286.89))))

(defn typical-proppant-mass
  ([] (let [mass-unit (rand-mass-unit)]
        [(typical-proppant-mass mass-unit) mass-unit]))
  ([mass-unit]
   (condp = mass-unit
     :lb (tuc/draw-normal 4962.77 1375.72)
     :kg (tuc/draw-normal 135068.05 16421.44))))

(defn typical-median-pressure
  ([] (let [pressure-unit (rand-pressure-unit)]
        [(typical-median-pressure pressure-unit) pressure-unit]))
  ([pressure-unit]
   (condp = pressure-unit
     :psi (tuc/draw-normal 7569.89 663.65)
     :kPa (tuc/draw-normal 64.1635 7.0987)
     :MPa (/ (typical-median-pressure :kPa) 1000))))

(def substances [:gas :liquid :rock :metal])

(defn typical-density
  ([]
   (let [density-unit (rand-density-unit)
         substance    (rand-nth substances)]
     (typical-density density-unit substance)))
  ([density-unit-or-substance]
   (let [is-density-unit          (fn [to-test] (#{:kg-per-m3 :lb-per-cu-ft} to-test))
         is-substance             (fn [to-test] ((set substances) to-test))
         [density-unit substance] (cond
                                    (is-density-unit density-unit-or-substance)
                                    [density-unit-or-substance (rand-nth substances)]
                                    (is-substance density-unit-or-substance)
                                    [(rand-nth [:kg-per-m3 :lb-per-cu-ft]) density-unit-or-substance])]
     (typical-density density-unit substance)))
  ([density-unit substance]
   (condp = density-unit
     :lb-per-cu-ft (let [[density _ substance] (typical-density :kg-per-m3)]
                     [((convert-units-f :kg-per-m3 :lb-per-cu-ft) density) :lb-per-cu-ft substance])
     :kg-per-m3    (condp = substance
                     ;; Typical values for different substances taken from
                     ;; https://serc.carleton.edu/mathyouneed/density/index.html#:~:text=Typical%20densities%20for%20gasses%20are,or%207%20g%2Fcm3.
                     ;; accessed on 23-Dec-2020. Additionally, the data for metals was taken from
                     ;; https://theengineeringmindset.com/density-of-metals/ accessed on 23-Dec-2020.
                     (let [density (condp = substance
                                     :gas    (tuc/draw-normal 1 0.11)
                                     :liquid (tuc/draw-normal 1000 110)
                                     :rock   (tuc/draw-normal 3000 330)
                                     :metal  (tuc/draw-normal) 10355 5431)]
                       [density density-unit substance])))))

(defn typical-total-pump-energy
  ([]
   (typical-total-pump-energy (rand-energy-unit)))
  ([energy-unit]
   (condp = energy-unit
     :ft-lb [(tuc/draw-normal 4.340741e10 1.443922e10) :ft-lb]
     :J     [(tuc/draw-normal 14670.972535 5275.739520) :J])))

(defn typical-force
  ([] (typical-force (rand-force-unit)))
  ([force-unit]
   ;; Retrieved from http://www.excoresources.com/appalachia/ and
   ;; https://en.wikipedia.org/wiki/Casing_string on 28-Dec-2020.
   (let [typical-casing-outer-diameter-inches (+ 5 (/ 1 2))
         typical-casing-pipe-witdth-inches    0.545
         typical-casing-inner-diameter-inches (- typical-casing-outer-diameter-inches
                                                 typical-casing-pipe-witdth-inches)
         typical-casing-area-sq-inches        (* 2 Math/PI (/ typical-casing-inner-diameter-inches 2))
         typical-pressure-psi                 (typical-surface-treating-pressure :psi)
         typical-force-lbf                    (* typical-pressure-psi typical-casing-area-sq-inches)]
     (condp = force-unit
       :lbf [typical-force-lbf force-unit]
       :N   [((convert-units-f :lbf :N) typical-force-lbf) force-unit]))))

(defn typical-power
  ([] (typical-power (rand-power-unit)))
  ([power-unit]
   (let [typical-energy (first (typical-total-pump-energy :J))
         typical-power  (condp = power-unit
                          :W  typical-energy ;; Assume energy expended for 1 s
                          :hp ((convert-units-f :W :hp) (first (typical-power :W))))]
     [typical-power power-unit])))
