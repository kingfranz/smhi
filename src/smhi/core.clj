; Prep RaspberryPi
; dd bs=8192 if=/home/soren/tmp/2016-09-23-raspbian-jessie.img of=/dev/sdc
; pi: enable wifi
; pi: hostname piclock
; pi: expand filesystem
; pi: wait for network
; pi: underscan disabled
; pi: enable VNC
; pi: set locale, timezone, keyboard, country
; pi: panel settings
; pi: static ip 192.168.0.45
; pi: disable screen blanking http://www.geeks3d.com/hacklab/20160108/how-to-disable-the-blank-screen-on-raspberry-pi-raspbian/
; pi: install tightvncserver
; pi: .profile
; pi:     if [ -n "$SSH_CLIENT" ] || [ -n "$SSH_TTY" ]; then
; pi:         SESSION_TYPE=remote/ssh
; pi:     else
; pi:         case $(ps -o comm= -p $PPID) in
; pi:             sshd|*/sshd) SESSION_TYPE=remote/ssh;;
; pi:         esac
; pi:     fi
; pi:     VNC_FOUND=`netstat -ln | awk '$1=="tcp" && $4~/59[0-9][0-9]$/ {print $4}'`
; pi:     if [ -z "$SESSION_TYPE" ] && [ -z "$VNC_FOUND" ]; then
; pi:         java -jar smhi-0.1.0-SNAPSHOT-standalone.jar
; pi:     fi
; vncviewer -geometry 1920x1080 -depth 24 piclock.soahojen:1

(ns smhi.core
    (:require [smhi.utils        :refer :all])
    (:require [smhi.graph-utils        :refer :all])
    (:require [smhi.spec        :refer :all])
    (:require [smhi.config        :refer :all])
    (:require [smhi.sun        :refer :all])
    (:require [clojure.data.json          :as json])
    (:require [clojure.java.io            :as io])
    (:require [clojure.spec               :as s])
    (:require [clj-time.core              :as t])
    (:require [clj-time.format            :as f])
    (:require [clj-time.local             :as l])
    (:require [clojure.math.numeric-tower :as math])
    (:require [seesaw.timer               :as st :except boolean?])
    (:require [seesaw.core        :refer :all])
    (:require [seesaw.border        :refer :all])
    (:require [seesaw.graphics        :refer :all])
    (:require [seesaw.color        :refer :all])
    (:require [seesaw.font        :refer :all])
    (:require [org.httpkit.client         :as http])
    (:require [taoensso.timbre            :as timbre
               :refer [log  trace  debug  info  warn  error  fatal  report
                       logf tracef debugf infof warnf errorf fatalf reportf spy get-env]])
    (:require [taoensso.timbre.appenders.core :as appenders])
    (:import (javax.swing JFrame JLabel)
             (java.awt Color Font FontMetrics GraphicsEnvironment)
             (java.io ByteArrayInputStream)
    		 (javax.imageio ImageIO)
    		 (java.io File))
    (:gen-class))

; the current weather forecast
(def weather-data (atom nil))

; not nil if we got an exception
(def weather-exception (atom nil))

; queue of radar images
(def radar-queue (atom nil))

; the current radar queue
(def radar-data (atom nil))

; not nil if we got an exception
(def radar-exception (atom nil))

; filter SMHI data based on one specific unit
(defn filter-param
	[param units]
	(let [param-key (keyword (:name param))]
		(if (:keep (param-key units))
			{param-key (first (:values param))}
			{})))

; filter SMHI data based on the units map
(defn filter-param-map
  [params units]
  (apply merge (map #(filter-param % units) params)))

(defn mk-weather-url
    [config]
    (str (get config :weather-url)
     "/api/category/"      (get config :category)
     "/version/"           (get config :version)
     "/geotype/point/lon/" (get config :longitude)
     "/lat/"               (get config :latitude)
     "/data.json"))

; send a weather forecast request to SMHI
(defn send-weather-request
    [config]
    (let [forecast (send-json-request (mk-weather-url config))]
        (if (= (s/conform smhi-spec forecast) :clojure.spec/invalid)
            (do
                (error "------- Invalid SMHI data -----------")
                (error (s/explain-str smhi-spec forecast))
                (error "-------------------------------------")
                (error forecast)
                (throw (Exception. "Invalid SMHI data"))))
        forecast))

; get latest radar image from SMHI
(defn get-radar-image
    [url]
    (let [data    (send-request url :image)
          img     (byte-array-2-image data)
          imgtype (java.awt.image.BufferedImage/TYPE_INT_ARGB)
          width   (.getWidth img)
          height  (.getHeight img)
          simg    (java.awt.image.BufferedImage. width height imgtype)
          g2d     (.createGraphics simg)]
     (.drawImage g2d img 0 0 width height nil)
     (.dispose g2d)
     ;(ImageIO/write simg "png"  (File. (str "radar-" (now-str) ".png")))
     simg))

; draw a (possibly scaled) image within a widget
(defn draw-image
	[widget ^java.awt.Graphics2D g2d image match valign halign iname]
	{:pre [(some #{match}  '(:both :width :height :min :max))
	(some #{valign} '(:top :center :bottom))
	(some #{halign} '(:left :center :right))]}
	(let [width           (.getWidth widget)
		height          (.getHeight widget)
		image-width     (.getWidth image)
		image-height    (.getHeight image)
		width-scale-1   (/ width image-width)
		height-scale-1  (/ height image-height)
		width-scale-2   (cond
			(= match :both)   width-scale-1
			(= match :width)  width-scale-1
			(= match :height) 1.0
			(= match :min)    (min width-scale-1 height-scale-1)
			(= match :max)    (max width-scale-1 height-scale-1))
		height-scale-2  (cond
			(= match :both)   height-scale-1
			(= match :width)  1.0
			(= match :height) height-scale-1
			(= match :min)    (min width-scale-1 height-scale-1)
			(= match :max)    (max width-scale-1 height-scale-1))
		new-width       (* image-width width-scale-2)
		new-height      (* image-height height-scale-2)
		width-offset    (cond
			(= halign :left)   0
			(= halign :center) (/ (- width new-width) 2)
			(= halign :right)  (- width new-width))
		height-offset   (cond
			(= valign :top)    0
			(= valign :center) (/ (- height new-height) 2)
			(= valign :bottom) (- height new-height))]
		(push g2d
			(draw (scale g2d width-scale-2 height-scale-2)
				(image-shape (/ width-offset width-scale-2) (/ height-offset height-scale-2) image)
				nil))))

; calculate all the values needed for drawing the temerature graph
(defn get-temp-scaling
	[data top bottom]
	(let [min-temp       (apply min (map second data))
		  max-temp       (apply max (map second data))
		  height         (inc (- bottom top))
		  real-temp-span (- max-temp min-temp)
		  half-diff      (/ (- tot-temp-span real-temp-span) 2)
		  start          (int (math/floor (- min-temp half-diff)))
          temp-info      {:min         min-temp                    ; lowest value
                          :max         max-temp                    ; highest value
                          :axis-scale  (/ tot-temp-span axis-span) ; integer size of steps
                          :axis-start  start                       ; what integer does the axis start on
                          :multiplier  1                           ; multiplier for data
                          :y-scale     (/ height tot-temp-span)        ; scale factor for window
                          :temp-span   real-temp-span              ; lowest to highest value
                          :min-padding (- min-temp start)}]        ; how much to pad values with
        temp-info))

; extract all the data for a specific parameter
(defn get-param
  [data param]
  (map #(vector (first %) (-> % second param)) data))

; draw the overall background
(defn draw-background
    [widget ^java.awt.Graphics2D g2d]
    (try
        (lbl-info widget g2d)
        (if-not (nil? @landscape-pic)
            (draw-image widget g2d @landscape-pic :both :center :center "background"))
        (catch Exception e
            (error (Exception. e)))))

(defn draw-exception-txt
    [widget ^java.awt.Graphics2D g2d e]
    (let [txt      (.getMessage e)
          width    (.getWidth widget)
          height   (.getHeight widget)
          t-width  (string-width  g2d exception-style txt)
          t-height (string-height g2d exception-style)]
        (draw g2d
            (string-shape (/ (- width t-width) 2)
                          (/ (- height t-height) 2)
                          txt)
            exception-style)))

(defn mk-radar-image
	[radar-pic map-pic sub-ul-x sub-ul-y sub-width sub-height]
	(let [map-width    (.getWidth map-pic)
          map-height   (.getHeight map-pic)
          border-size  10
          sub-radar    (.getSubimage radar-pic
                                     sub-ul-x
                                     sub-ul-y
                                     sub-width
                                     sub-height)
          width-ratio  (/ (- map-width  (* border-size 2)) sub-width)
          height-ratio (/ (- map-height (* border-size 2)) sub-height)
          buffer       (buffered-image map-width map-height)
          buffer-g2d   (.createGraphics buffer)]
        (-> buffer-g2d (draw (image-shape 0 0 map-pic) nil)
                       (scale width-ratio height-ratio)
                       (draw (image-shape 1 1 sub-radar) nil))
        buffer))

(defn have-radar-data
	[]
	(if (and (not-nil? @radar-data) (not-empty @radar-data))
		true
		(do
			(when-let [queue @radar-queue]
				(set-var radar-data (apply conj queue (repeat (* radar-fps radar-ani-delay-sec) (last queue)))))
			(and (not-nil? @radar-data) (not-empty @radar-data)))))

(defn get-current-radar-image
	[]
	(if (have-radar-data)
		(let [image (peek @radar-data)]
			(set-var radar-data (pop @radar-data))
			image)
		nil))

(defn add-radar-2-queue
	[image]
	(if (nil? @radar-queue)
		(set-var radar-queue (clojure.lang.PersistentQueue/EMPTY)))
	(if (> (count @radar-queue) max-radar-queue-size)
		(set-var radar-queue (pop @radar-queue)))
	(set-var radar-queue (conj @radar-queue image)))

; draw the radar picture
(defn draw-radar
  [widget ^java.awt.Graphics2D g2d]
  (try
    (lbl-info widget g2d)
    (when-let [image (get-current-radar-image)]
    	(draw g2d (image-shape 0 0 image) nil))
    (if (not (nil? @radar-exception))
        (draw-exception-txt widget g2d @radar-exception))
    (catch Exception e
    	(error (Exception. e)))))

; draw the clock
(defn draw-clock
  [widget ^java.awt.Graphics2D g2d]
  (try
    (lbl-info widget g2d)
    ;(info "tick")
    (let [now          (l/local-now)
            minute       (+ (t/minute now) (/ (t/second now) 60))
            hour         (+ (t/hour now) (/ minute 60))
            rotated-hour (center-rotate hour-hand (hour-to-angle hour))
            rotated-min  (center-rotate min-hand (* minute 6))
            rotated-sec  (center-rotate sec-hand (* (t/second now) 6))]
         (draw-image widget g2d clock-pic    :min :center :center "clock")
         (draw-image widget g2d rotated-hour :min :center :center "hour")
        (draw-image widget g2d rotated-min  :min :center :center "minute")
        (draw-image widget g2d rotated-sec  :min :center :center "second"))
    (catch Exception e
     (error (Exception. e)))))

; draw graphic forecast of rain
(defn draw-rain
  [^java.awt.Graphics2D g2d data top bottom]
  (let [rain-data  (get-param data :pmedian)
        height     (inc (- bottom top))
        y-scale    (/ height max-rain-level)
        points     (map #(vector (first %) (- bottom (-> % second (* y-scale)))) rain-data)
        all-points (concat [[(-> points first first) bottom]]
                    points
                    [[(-> points last first) bottom]])]
    (draw g2d
          (apply polygon all-points)
          rain-style)
    (draw-line-seq g2d points (style :foreground :white))))

; draw graphic forecast of temperature
(defn draw-temp
  [^java.awt.Graphics2D g2d data info top bottom]
  (let [points (map #(vector (first %)
                      (- bottom (-> % second
                                 (- (:min         info))
                                 (* (:multiplier  info))
                                 (+ (:min-padding info))
                                 (* (:y-scale     info)))))
                data)]
    (draw-line-seq g2d points temp-style)))

; draw graphic forecast of wind
(defn draw-wind
  [^java.awt.Graphics2D g2d data top bottom]
  (let [w-speed   (get-param data :ws)
        w-gust    (get-param data :gust)
        height    (inc (- bottom top))
        max-gust  (+ (apply max (map second w-gust)) wind-padding)
        y-scale   (/ height (* axis-span wind-axis-factor))
        g-points  (map #(vector (first %) (- bottom (-> % second (* y-scale)))) w-gust)
        s-points  (map #(vector (first %) (- bottom (-> % second (* y-scale)))) (reverse w-speed))]
    (draw g2d
          (apply polygon (concat g-points s-points))
          wind-style)))

; draw text with a circle background
(defn draw-text
  [^java.awt.Graphics2D g2d x y txt txt-style left-side]
  (let [txt-width    (string-width g2d txt-style txt)
        txt-height   (string-height g2d txt-style)
        radius       (+ (/ txt-height 2) 4)
        circle-x     (if left-side (- x (/ radius 2) 5) (+ x (/ radius 2) 5))
        txt-y        (+ y (/ txt-height 4))
        txt-x        (- circle-x (/ txt-width 2))]
    (draw g2d
        (circle circle-x y radius)
        text-circle-style)
    (draw g2d
        (string-shape txt-x txt-y txt)
        txt-style)))

; draw axises for forecast graphics (and scales)
(defn draw-axis
  [^java.awt.Graphics2D g2d data width top bottom temp-info]
  (let [left-x       (dec left-axis-width)
        right-x      (- width right-axis-width)
        graph-height (inc (- bottom top))
        day-width    (/ (- right-x left-x) graph-days)
        rain-text-x  (+ (- width right-axis-width) tick-width 5)
        temp-start   (:axis-start temp-info)
        temp-scale   (:axis-scale temp-info)
        height-scale (/ graph-height axis-span)
        font-metrics (.getFontMetrics g2d axis-txt-font)]
    ; left vertical line
    (draw g2d
      (line left-x top
          left-x bottom)
      axis-style)
    ; left left vertical line
    (draw g2d
      (line (/ left-x 2) top
          (/ left-x 2) bottom)
      axis-style)
    ; right vertical line
    (draw g2d
      (line right-x top
          right-x bottom)
      axis-style)
    ; bottom line
    (draw g2d
      (line left-x bottom
          right-x bottom)
      axis-style)
    ; zero line
    (if (and (< temp-start 0) (> temp-start (- 0 tot-temp-span)))
      (draw g2d
        (line left-x  (- bottom (* (- 0 temp-start) (/ graph-height tot-temp-span)))
            right-x (- bottom (* (- 0 temp-start) (/ graph-height tot-temp-span))))
        zero-line-style))
    ; day lines
    (doseq [day-line (range 1 graph-days)]
      (draw g2d
        (line (+ left-x (* day-line day-width)) top
            (+ left-x (* day-line day-width)) bottom)
        day-tick-style))
    ; ticks on axises
    (doseq [tick-idx (range 0 axis-span 1)]
      ; temp axis
      (draw g2d
        (line left-x
            (- bottom (* tick-idx height-scale))
            (- left-x (if (even? tick-idx) tick-width (/ tick-width 2)))
            (- bottom (* tick-idx height-scale)))
        axis-style)
      ; wind axis
      (draw g2d
        (line right-x
            (- bottom (* tick-idx height-scale))
            (+ right-x (if (even? tick-idx) tick-width (/ tick-width 2)))
            (- bottom (* tick-idx height-scale)))
        axis-style)
      ; rain axis
      (draw g2d
        (line (/ left-x 2)
            (- bottom (* tick-idx height-scale))
            (- (/ left-x 2) (if (even? tick-idx) tick-width (/ tick-width 2)))
            (- bottom (* tick-idx height-scale)))
        axis-style))
    ; axis text
    (doseq [text-idx (range 2 axis-span 2)]
      (draw-text g2d
             (- left-x tick-width)
             (- bottom (* text-idx height-scale))
             (format "%d" (+ temp-start (* temp-scale text-idx)))
             temp-axis-text-style
             true)
      (draw-text g2d
             (+ right-x tick-width)
             (- bottom (* text-idx height-scale))
             (format "%d" (* text-idx wind-axis-factor))
             wind-axis-text-style
             false)
      (draw-text g2d
             (- (/ left-x 2) tick-width)
             (- bottom (* text-idx height-scale))
             (format "%d" (/ text-idx (/ axis-span rain-axis-span)))
             rain-axis-text-style
             true))))

; draw axises for forecast graphics (and scales)
(defn draw-sunrise
	[^java.awt.Graphics2D g2d left-x left-y width height]
	(try
		(when-let [info (get-sun-info)]
			(info "drawing sunrise and sunset")
			(let [sec-2-x              (fn [s] (* (/ width (* graph-days 24 60 60)) s))
		          dt-to-sec            (fn [x] (+ (* (t/hour x) 60 60) (* (t/minute x) 60) (t/second x)))
		          twilight-start-sec   (dt-to-sec (->> info :results twilight-begin f/parse))
		          sunrise-sec          (dt-to-sec (->> info :results :sunrise f/parse))
		          sunset-sec           (dt-to-sec (->> info :results :sunset f/parse))
		          twilight-end-sec     (dt-to-sec (->> info :results twilight-end f/parse))
		          day-start            (fn [d] (+ left-x (sec-2-x (* d (* 24 60 60)))))
		          twilight-start-x     (sec-2-x twilight-start-sec)
		          sunrise-x            (sec-2-x sunrise-sec)
		          sunset-x             (sec-2-x sunset-sec)
		          twilight-end-x       (sec-2-x twilight-end-sec)
		          night-start-width    twilight-start-x
		          twilight-start-width (- sunrise-x twilight-start-x)
		          twilight-end-width   (- twilight-end-x sunset-x)
		          night-end-width      (sec-2-x (- (* 24 60 60) twilight-end-sec))
		          twilight-start       (fn [d] (+ (day-start d) twilight-start-x))
		          sunset-start         (fn [d] (+ (day-start d) sunset-x))
		          night-start          (fn [d] (+ (day-start d) twilight-end-x))
		          mk-gradient          (fn [xs w l] (style :background
			    									(linear-gradient :start [xs 0]
			    								 					 :end [(+ xs w) 0]
			    								 					 :colors [(color 30 30 30 (if l 255 0))
			    								 					 		  (color 30 30 30 (if l 0 255))])))

		          draw-morning-night (fn [d] (draw g2d (rect (day-start d)
		          											 left-y
		          											 night-start-width
		          											 height)
			    									(style :background (color 30 30 30))))
		          draw-morning-twilight (fn [d] (draw g2d
		          									  (rect (twilight-start d)
		          											left-y
		          											twilight-start-width
		          											height)
			    									  (mk-gradient (twilight-start d) twilight-start-width true)))
		          draw-evening-twilight (fn [d] (draw g2d
		          									  (rect (sunset-start d)
		          											left-y
		          											twilight-end-width
		          											height)
			    									  (mk-gradient (sunset-start d) twilight-end-width false)))
		          draw-evening-night (fn [d] (draw g2d (rect (night-start d)
		          											 left-y
		          											 night-end-width
		          											 height)
			    									   (style :background (color 30 30 30))))]
		        ;(println "sunset-sec:" sunset-sec "x:" (int (sec-2-x sunset-sec)))
		        (doseq [d (range graph-days)]
;			        (println (int (day-start d))
;			        		 (int night-start-width)
;			        		 (int (twilight-start d))
;			        		 (int twilight-start-width)
;			        		 (int (sunset-start d))
;			        		 (int twilight-end-width)
;			        		 (int (night-start d))
;			        		 (int night-end-width))
		        	(draw-morning-night d)
		        	(draw-morning-twilight d)
		        	(draw-evening-twilight d)
		        	(draw-evening-night d))))
		(catch Exception e
    		(error (Exception. "draw-sunrise")))))
    
(defn mk-symbol-targets
  []
  (flatten (for [d    (range 0 graph-days)
                 :let [d24 (* d 24 60)
                        x   [(+ d24 (* 6 60)) (+ d24 (* 12 60)) (+ d24 (* 18 60))]]]
            x)))

(defn pick-closer
  [[x1 y1] t [x2 y2]]
  (let [dist1 (abs (- t x1))
        dist2 (abs (- t x2))]
    (if (< dist1 dist2)
      y1
      y2)))

(defn find-closest
  "doc-string"
  [target symb-data]
  (loop [symbols symb-data]
    (if (empty? symbols)
      nil
      (if (or (> (-> symbols first first) target) (= (count symbols) 1))
        (-> symbols first second)
        (if (and (< (-> symbols first first) target) (> (-> symbols second first) target))
          (pick-closer (first symbols) target (second symbols))
          (recur (rest symbols)))))))

; draw the weather symbols on the forecast
(defn draw-graph-symbols
  [^java.awt.Graphics2D g2d data top width left-side]
  (let [symb-data     (get-param data :Wsymb)
        symb-values   (map #(find-closest % symb-data) (mk-symbol-targets))
        symbs-per-day 3
        day-width     (/ width graph-days)
        symbol-width  (.getWidth (get tiny-symbol-pics 0))
        symbol-offset (/ (- day-width (* symbol-width symbs-per-day)) (inc symbs-per-day))]
    (doseq [day (range graph-days)]
      (doseq [symb-idx (range symbs-per-day)]
        (let [x   (+ left-side (* day-width day) symbol-offset (* symb-idx (+ symbol-width symbol-offset)))
              y   top
              ii  (nth symb-values (+ (* day symbs-per-day) symb-idx))
              img (get tiny-symbol-pics ii)]
          (draw g2d (image-shape x top img) nil))))))

(defn draw-dates
  [^java.awt.Graphics2D g2d top height left-side width]
  (let [date-strings (mk-date-strings)
        day-width    (/ width graph-days)]
    (doseq [date-idx (range graph-days)
            :let [dstr-width (string-width g2d date-txt-style (nth date-strings date-idx))
                  dstr-height (string-height g2d date-txt-style)]]
      (draw g2d
          (string-shape (- (+ left-side (/ day-width 2) (* day-width date-idx)) (/ dstr-width 2))
                  (+ top (/ dstr-height 2) (/ height 2) -3)
                  (nth date-strings date-idx))
          date-txt-style))))

; draw the forecast graphics
(defn draw-curve
  [widget ^java.awt.Graphics2D g2d]
  (try
    (lbl-info widget g2d)
    (if (not (nil? @weather-data))
        (let [width        (.getWidth widget)
              height       (.getHeight widget)
              date-height  30
              top          axis-width
              bottom       (- height date-height axis-width)
              graph-height (- bottom top)
              width-avail  (- width left-axis-width right-axis-width)
              x-scale      (/ width-avail week-minutes)
              x-data       (map #(vector (-> % first (* x-scale) (+ left-axis-width) int) (second %)) @weather-data)
              temp-data    (map #(vector (first %) (->> % second (* 1.0))) (get-param x-data :t))
              temp-info    (get-temp-scaling temp-data top bottom)]
          (fill g2d (color 128 128 128 128) width height)
          (draw-sunrise g2d left-axis-width top width-avail (inc (- bottom top)))
          (draw-axis g2d x-data width top bottom temp-info)
          (draw-rain g2d x-data top (- bottom 3))
          (draw-wind g2d x-data top bottom)
          (draw-temp g2d temp-data temp-info top bottom)
          (draw-graph-symbols g2d @weather-data top width-avail left-axis-width)
          (draw-dates g2d (- height date-height) date-height left-axis-width width-avail)))
    (if (not (nil? @weather-exception))
        (draw-exception-txt widget g2d @weather-exception))
    (catch Exception e
      (info (Exception. "draw-curve") "arg1" "arg2"))))

; draw the compass and wind direction
(defn draw-wind-dir
  [widget ^java.awt.Graphics2D g2d]
  (try
    (lbl-info widget g2d)
    (if (not (nil? @weather-data))
        (let [direction      (-> @weather-data first second :wd)
                rotated-arrow  (center-rotate arrow-pic (mod (+ direction 180) 360))]
         (draw-image widget g2d compass-pic :min :center :center "compass")
         (draw-image widget g2d rotated-arrow :min :center :center "arrow")))
    (catch Exception e
      (error (Exception. e)))))

; draw the symbol for current weather
(defn draw-w-symbol
  [widget ^java.awt.Graphics2D g2d]
  (try
    (lbl-info widget g2d)
    (if (not (nil? @weather-data))
        (let [symb-num (-> @weather-data first second :Wsymb)]
         (draw-image widget g2d (get symbol-pics symb-num) :min :center :center "symbol")))
    (catch Exception e
      (error (Exception. e)))))

; draw background, title and value for one section of now info
(defn draw-info-text
  [^java.awt.Graphics2D g2d center-x width top height title title-style value value-style]
  (let [t-width       (string-width  g2d title-style title)
        t-height      (string-height g2d title-style)
        v-width       (string-width  g2d value-style (str value))
        v-height      (string-height g2d value-style)
        top-border    9
        center-border 6
        bottom-border 14
        side-border   10
        title-part    1/3
        value-part    2/3
        title-height  (* height title-part)
        value-height  (* height value-part)]
    ; draw top sq
    (draw g2d
      (rounded-rect (+ (- center-x (/ width 2)) side-border)
              (+ top top-border)
              (- width (* side-border 2))
              (- title-height top-border center-border)
              10 10)
      info-bg-style)
    ; draw bottom sq
    (draw g2d
      (rounded-rect (+ (- center-x (/ width 2)) side-border)
              (+ top title-height)
              (- width (* side-border 2))
              (- value-height bottom-border)
              10 10)
      info-bg-style)
    ; draw top txt
    (draw g2d
        (string-shape (- center-x (/ t-width 2))
                (+ top (/ title-height 2) (/ t-height 2) -3)
                title)
        title-style)
    ; draw bottom txt
    (draw g2d
        (string-shape (- center-x (/ v-width 2))
                (+ top title-height (/ value-height 2) (/ v-height 2) -19)
                value)
        value-style)))

; draw all the parts of the now info
(defn draw-info
  [widget ^java.awt.Graphics2D g2d]
  (try
    (if (not (nil? @weather-data))
      (let [w-width      (.getWidth widget)
              w-height     (.getHeight widget)
              num-columns  5
              num-rows     2
              column-width (/ w-width num-columns)
              row-height   (/ w-height num-rows)
              center-x     (fn [x] (+ (/ column-width 2) (* column-width x)))
              v-frmt       (fn [k] (-> @weather-data first second k))
              dit          (fn [x y t v] (draw-info-text g2d
                                          (center-x x)
                                          column-width
                                          (* y row-height)
                                          row-height
                                          t
                                          info-title-style
                                          v
                                          info-value-style))
              cloud-cover  (-> (v-frmt :tcc_mean) (/ 8) (* 100) int)]
        (dit 0 0 "Temp"           (str (v-frmt :t) degree-char))
        (dit 1 0 "Humidity"       (str (v-frmt :r) "%"))
        (dit 2 0 "Cloud"          (str cloud-cover "%"))
        (dit 3 0 "Wind Speed m/s" (str (v-frmt :ws) "-" (v-frmt :gust)))
        (dit 0 1 "Pressure"       (str (v-frmt :msl)))
        (dit 1 1 "Rain mm/h"      (str (v-frmt :pmedian)))
        (dit 2 1 "Thunder"        (str (v-frmt :tstm) "%"))
        (dit 3 1 "Wind Dir"       (wind-dir-to-str (v-frmt :wd)))))
    (catch Exception e
      (error (Exception. e)))))

; create the frame
(def smhi-frame
  (window
    :width horiz-res
    :height vert-rez
    :content
      (xyz-panel :items [
                         (label  :id     :lbl-symbol
                             :bounds    [(* wnow-width 4) (* wnow-height 0) wnow-width wnow-height]
                             :foreground lbl-txt-color
                             :paint     draw-w-symbol)
                         (label  :id     :wind-dir
                             :bounds    [(* wnow-width 4) (* wnow-height 1) wnow-width wnow-height]
                             :foreground lbl-txt-color
                             :paint     draw-wind-dir)

                         (label  :id     :info  ; info
                             :bounds    [0 0 info-width info-height]
                             :paint     draw-info)
                         (label  :id     :radar  ; radar
                             :bounds    [0 info-height radar-width radar-height]
                             :paint     draw-radar)
                         (label  :id     :clock  ; the clock
                             :bounds    [radar-width 0 clock-width clock-height]
                             :listen     [:mouse-clicked (fn [e] (java.lang.System/exit 0))]
                             :paint     draw-clock)
                         (label  :id     :forecast  ; forecast graphics
                             :bounds    [0 clock-height graphics-width graphics-height]
                             :paint     draw-curve)
                         (label   :id     :lbl-back ; background
                             :bounds    [0 0 horiz-res vert-rez]
                             :paint     draw-background)])))

; convert the SMHI timestamp to minutes from midnight (or -1 if it's too old)
(defn mk-delta-time
  [m now day-start]
  (let [map-time    (f/parse (:validTime m))]
    (if (t/before? map-time now)
      -1
      (t/in-minutes (t/interval day-start map-time)))))

; interpolate a Y value between 2 X,Y pairs
(defn mk-intermediate-value
  [x1 y1 x2 y2 target-x is-int]
  (let [k        (/ (- y2 y1) (- x2 x1))
        m        (- y1 (* k x1))
        target-y (+ (* k target-x) m)]
    (if is-int
      (int target-y)
      target-y)))

; create a new entry between prev-entry and next-entry
(defn mk-intermediate
  [prev-entry next-entry ts]
  (let [prev-ts   (first prev-entry)
        next-ts   (first next-entry)
        prev-map  (second prev-entry)
        next-map  (second next-entry)
        prev-keys (keys prev-map)]
    (loop [map-keys (keys prev-map)
           acc      {}]
      (if (empty? map-keys)
        [ts acc]
        (recur (rest map-keys)
             (assoc acc
                   (first map-keys)
                   (mk-intermediate-value prev-ts
                                (get prev-map (first map-keys))
                                next-ts
                                (get next-map (first map-keys))
                                ts
                                (get (get units (first map-keys)) :is-int))))))))

; process the SMHI data and convert timestamp to delta minutes
(defn process-data
	[]
	(let [now           (t/now)
		  day-start     (t/today-at 00 00)
		  response      (send-weather-request smhi-config)
		  resp          (:timeSeries response)
		  filtered-resp (map #(vector (mk-delta-time % now day-start)
									  (filter-param-map (:parameters %) units))
							 resp)
          valid-resp    (filter #(>= (first %) 0) filtered-resp) ; remove old ones
          week-resp     (filter #(< (first %) week-minutes) valid-resp)] ; only this week
        (if (> (count valid-resp) (count week-resp))
        	(let [next-entry (nth valid-resp (count week-resp))
        		  prev-entry (last week-resp)
        		  new-entry  (mk-intermediate prev-entry next-entry (dec week-minutes))]
        		(conj (vec week-resp) new-entry))
        	week-resp)))

(def weather-timer-ts (atom nil))

(defn time-for-forecast? 
	"doc-string"
	[]
	(let [now (l/local-now)]
		(or (nil? @weather-timer-ts) (> (t/in-minutes (t/interval @weather-timer-ts now)) 30))))

(defn weather-timer-fn
    [x]
    (try
    	(if (time-for-forecast?)
    		(do
		        (info "getting new forecast")
		        (let [weather (process-data)]
		            (info "weather-timer-fn: successfully got new forecast")
		            (set-var weather-data weather)
		            (set-var weather-exception nil)
		            (set-var weather-timer-ts (l/local-now))
		            (if (not (nil? @weather-data))
		                (repaint! [(select smhi-frame [:#info])
		                           (select smhi-frame [:#wind-dir])
		                           (select smhi-frame [:#lbl-symbol])
		                           (select smhi-frame [:#forecast])])))))
        (catch Exception e
            (do
                (set-var weather-exception e)
                (set-var weather-timer-ts nil)
                (error "---------------------------------------------------")
                (error "Error in: weather-timer-fn")
                (error (str "Exception: " (.getMessage e)))
                (error "---------------------------------------------------")
                (spit "clock-error.log" (str (now-str) " weather-timer-fn Exception: " (.getMessage e) "\n") :append true)
                (error (Exception. e))))))

(defn radar-timer-fn
    [x]
    (try
        (info "getting new radar image")
        (let [radar-pic (get-radar-image (:radar-url smhi-config))
        	  combo-pic (mk-radar-image radar-pic
        	  							map-pic
        	  							radar-sub-upper-left-x
        	  							radar-sub-upper-left-y
        	  							radar-sub-width
        	  							radar-sub-height)]
            (info "radar-timer-fn: successfully got new image")
            (add-radar-2-queue combo-pic)
            (set-var radar-exception nil)
            (set-background)
            (repaint! (select smhi-frame [:#lbl-back])))
        (catch Exception e
            (do
                (set-var radar-exception e)
                (error "---------------------------------------------------")
                (error "Error in: radar-timer-fn")
                (error (str "Exception: " (.getMessage e)))
                (error "---------------------------------------------------")
                (spit "clock-error.log" (str (now-str) " radar-timer-fn Exception: " (.getMessage e) "\n") :append true)
                (error (Exception. e))))))

(defn set-screen
    [the-frame args]
    (let [num-args (count args)
          arg-num  (if (and (> num-args 0) (is-pos-int-str? (first args))) (parse-int (first args)) 0)
          screens  (get-screens)
          screen-num (if (>= arg-num (count screens)) 0 arg-num)]
        (move! the-frame :to [(:x (nth screens screen-num)) (:y (nth screens screen-num))])))

(defn -main
    [& args]
    (timbre/merge-config!
        {:appenders {:spit (max-size-appender {:path "./" :prefix "clock" :max-size 1000000 :num-files 10
                                                  :output-fn (partial timbre/default-output-fn {:stacktrace-fonts {}})})}})
    (set-background)
    (-> smhi-frame (set-screen args) show!)

    ; set timer for clock 1s
    (st/timer (fn [x] (repaint! (select smhi-frame [:#clock]))))

    ; set timer for weather forecast 30 minutes
    (st/timer weather-timer-fn
        :initial-delay (* 1000 2)
        :delay (* 1000 60 30))

    ; set timer for radar image paint 100ms
    (st/timer  (fn [x] (repaint! (select smhi-frame [:#radar])))
        :initial-delay (* 1000 5)
        :delay (/ 1000 radar-fps))

    ; set timer for radar image download 5 minutes
    (st/timer radar-timer-fn
        :delay radar-interval-ms)
)
