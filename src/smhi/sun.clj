(ns smhi.sun
  (:require [smhi.utils                 :as utils]
            [smhi.spec                  :as spec]
            [smhi.config                :as conf]
            [clojure.data.json          :as json]
            [clojure.java.io            :as io]
            [clojure.spec               :as s]
            [clj-time.core              :as t]
            [clj-time.format            :as f]
            [clj-time.local             :as l]
            [seesaw.core                :as sc]
            [seesaw.border              :as sb]
            [seesaw.graphics            :as sg]
            [seesaw.color               :as sclr]
            [seesaw.font                :as sf]
            [org.httpkit.client         :as http]
            [taoensso.timbre            :as timbre]
            [taoensso.timbre.appenders.core :as appenders])
    (:import (javax.swing JFrame JLabel)
             (java.awt Color Font FontMetrics GraphicsEnvironment)
             (java.io ByteArrayInputStream)
             (java.lang.Math)))


(defn jdn-2-date
    [jdn]
    (let [y 4716
          j 1401
          m 2
          n 12
          r 4
          p 1461
          v 3
          u 5
          s 153
          w 2
          B 274277
          C -38
          div (fn [a b] (int (/ a b)))
          f (+ jdn j (div (* (div (+ (* jdn 4) B) 146097) 3) 4) C)
          e (+ (* r f) v)
          g (div (mod e p) r)
          h (+ (* u g) w)
          D (+ (div (mod h s) u) 1)
          M (+ (mod (+ (div h s) m) n) 1)
          Y (+ (div e p) (- 0 y) (div (+ n m (- 0 M)) n))]
        {:year Y :month M :day D}))

(defn sun-calc
    [year month day longitude latitude]
    (let [a (Math/floor (/ (- 14 month) 12))
          y (- (+ year 4800) (a month))
          m (+ month (* 12 (a month)) -3)
          JDN (+ day
                 (Math/floor (/ (+ (* 153 (m month)) 2)  5))
                 (* 365 (y year month))
                 (Math/floor (/ (y year month) 4))
                 (- 0 (Math/floor (/ (y year month) 100)))
                 (Math/floor (/ (y year month) 400))
                 -32045)
          n (+ (- (JDN year month day) 2451545.0) 0.0008)
          j-star (- (n year month day) (/ longitude 360.0))
          solar-mean (mod (+ (* j-star 0.98560028) 357.5291) 360)
          eq-center (+ (* 1.9148 (Math/sin solar-mean))
                       (* 0.0200 (Math/sin (* 2 solar-mean)))
                       (* 0.0003 (Math/sin (* 3 solar-mean))))
          eclip-long (mod (+ solar-mean eq-center 180 102.9372) 360.0)
          solar-noon (+ 2451545.5 j-star (* 0.0053 (Math/sin solar-mean)) (- 0 (* 0.0069 (Math/sin (* 2 eclip-long)))))
          sun-decl (Math/asin (* (Math/sin eclip-long) (Math/sin 23.44)))
          hour-angle (Math/acos (/ (- (Math/sin -0.83)
                             (* (Math/sin latitude)
                                (Math/sin sun-decl)))
                          (* (Math/cos latitude) 
                             (Math/cos sun-decl))))
          sunrise (+ solar-noon (/ hour-angle 360))
          sunset (- (solar-noon year month day longitude) (/ hour-angle 360))
          jdn-rise (sunrise year month day longitude latitude)
          jdn-set (sunset year month day longitude latitude)
          rise-h (* (- jdn-rise (int jdn-rise)) 24)
          rise-m (* (- rise-h (int rise-h)) 60)
          set-h (* (- jdn-set (int jdn-set)) 24)
          set-m (* (- set-h (int set-h)) 60)
        ]
        {:rise-h (int rise-h) :rise-m (int rise-m) :set-h (int set-h) :set-m (int set-m)}))

(def sun-info (atom nil))

(defn pp-sun
    [sun f]
    (let [to-txt (fn [x] (f/unparse (f/with-zone (f/formatter :hour-minute) (t/default-time-zone)) x))
          t-str  (fn [x] (->> sun :results x f/parse to-txt))]
        (f (str "astronomical_twilight_begin: " (t-str :astronomical_twilight_begin)))
        (f (str "nautical_twilight_begin:     " (t-str :nautical_twilight_begin)))
        (f (str "civil_twilight_begin:        " (t-str :civil_twilight_begin)))
        (f (str "sunrise:                     " (t-str :sunrise)))
        (f (str "solar_noon:                  " (t-str :solar_noon)))
        (f (str "day_length:                  " (format "%.1f" (float (/ (->> sun :results :day_length) 3600)))))
        (f (str "sunset:                      " (t-str :sunset)))
        (f (str "civil_twilight_end:          " (t-str :civil_twilight_end)))
        (f (str "nautical_twilight_end:       " (t-str :nautical_twilight_end)))
        (f (str "astronomical_twilight_end:   " (t-str :astronomical_twilight_end)))
    ))

(defn send-sun-request
	[]
	(timbre/info "getting new sun info")
	(try
		(let [url (str "http://api.sunrise-sunset.org/json?lat=" (:latitude @conf/config)
					   "&lng=" (:longitude @conf/config) "&formatted=0")
			  response (utils/send-json-request url)]
			(timbre/info "successfully got new sun info")
			(if (= (s/conform spec/sunrise-spec response) :clojure.spec/invalid)
	            (do
	                (timbre/error "------- Invalid Sunrise data -----------")
	                (timbre/error (s/explain-str spec/sunrise-spec response))
	                (timbre/error "-------------------------------------")
	                nil)
				(let [new-info (assoc response :timestamp (l/local-now))]
                    (pp-sun new-info println)
                    new-info)))
		(catch Exception e
            (do
                (timbre/error "---------------------------------------------------")
                (timbre/error "Error in: send-sun-request")
                (timbre/error (str "Exception: " (.getMessage e)))
                (timbre/error "---------------------------------------------------")
                nil))))

(defn get-sun-info
	[]
	(if (or (nil? @sun-info) (> (t/in-minutes (t/interval (:timestamp @sun-info) (l/local-now))) (* 12 60)))
		(reset! sun-info (send-sun-request)))
	@sun-info)

(defn inprint-image
	[image]
	(if-let [sun-info (get-sun-info)]
		(let [g2d (.createGraphics image)
			  to-txt (fn [x] (f/unparse (f/with-zone (f/formatter :hour-minute) (t/default-time-zone)) x))
			  rise-txt (->> sun-info :results :sunrise f/parse to-txt)
			  set-txt  (->> sun-info :results :sunset f/parse to-txt)
              up-down-txt (format "↑ %s  ↓ %s" rise-txt set-txt)
			  up-down-width (utils/string-width g2d conf/sun-style up-down-txt)
              date-txt (f/unparse (f/formatter "EEEE dd MMM") (l/local-now))
              date-width (utils/string-width g2d conf/sun-style date-txt)
			  sq-width  400
			  sq-height 60
			  sq-radius 50]
			(sg/draw g2d
    			(sg/rounded-rect (- (:x conf/sun-point) (/ sq-width 2))
            				  (- (:y conf/sun-point) 50)
            				  sq-width
            				  sq-height
            				  sq-radius
            				  sq-radius)
    			conf/sun-bg-style)
			(sg/draw g2d
            	  (sg/string-shape (- (:x conf/sun-point) (/ up-down-width 2))
                				(- (:y conf/sun-point) 8)
                        		up-down-txt)
            	  conf/sun-style)
			(sg/draw g2d
    			(sg/rounded-rect (- (:x conf/date-point) (/ sq-width 2))
            				  (- (:y conf/date-point) 50)
            				  sq-width
            				  sq-height
            				  sq-radius
            				  sq-radius)
    			conf/sun-bg-style)
			(sg/draw g2d
            	  (sg/string-shape (- (:x conf/date-point) (/ date-width 2))
                				(- (:y conf/date-point) 8)
                        		date-txt)
            	  conf/sun-style)
			(.dispose g2d)))
	image)

;(clj-time.format/unparse (clj-time.format/formatter "EEE dd MMM") (clj-time.local/local-now))