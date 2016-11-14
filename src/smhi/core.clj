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
	(:require [clojure.data.json          :as json])
	(:require [clojure.java.io            :as io])
	(:require [clj-time.core              :as t])
	(:require [clj-time.format            :as f])
	(:require [clj-time.local             :as l])
	(:require [clojure.math.numeric-tower :as math])
	(:require [seesaw.timer               :as st])
	(:use seesaw.core
		  seesaw.border
		  seesaw.graphics
		  seesaw.color
		  seesaw.font)
	(:import (javax.swing JFrame JLabel)
             (java.awt Color Font FontMetrics GraphicsEnvironment))
  	(:gen-class))

(def smhi-config
	{:latitude    "58.786869"
	 :longitude   "14.265020"
	 :weather-url "http://opendata-download-metfcst.smhi.se"
	 :radar-url   "http://opendata-download-radar.smhi.se/api/version/latest/area/sweden/product/comp/latest.png"
	 :category    "pmp2g"
	 :version     "2"})

(def units
	{:msl      {:str "Air Pressure"            :is-int false :keep true  :unit "hPa"}
	 :t        {:str "Temperature"             :is-int false :keep true  :unit "C"}
	 :vis      {:str "Visibility"              :is-int false :keep false :unit "km"}
	 :wd       {:str "Wind Dir"                :is-int true  :keep true  :unit "degree"}
	 :ws       {:str "Wind Speed"              :is-int false :keep true  :unit "m/s"}
	 :r        {:str "Humidity"                :is-int true  :keep true  :unit "%"}
	 :tstm     {:str "Thunder"                 :is-int true  :keep true  :unit "%"}
	 :tcc_mean {:str "Total Clouds"            :is-int true  :keep true  :unit "octas"}
	 :lcc_mean {:str "Low Clouds"              :is-int true  :keep false :unit "octas"}
	 :mcc_mean {:str "Medium Clouds"           :is-int true  :keep false :unit "octas"}
	 :hcc_mean {:str "High Clouds"             :is-int true  :keep false :unit "octas"}
	 :gust     {:str "Gust Speed"              :is-int false :keep true  :unit "m/s"}
	 :pmin     {:str "Min Precip"              :is-int false :keep true  :unit "mm/h"}
	 :pmax     {:str "Max Precip"              :is-int false :keep true  :unit "mm/h"}
	 :spp      {:str "Percent Frozen"          :is-int true  :keep true  :unit "%"}
	 :pcat     {:str "Precip Category"         :is-int true  :keep true  :unit "category"}
	 :pmean    {:str "Mean Precip Intensity"   :is-int false :keep true  :unit "mm/h"}
	 :pmedian  {:str "Median Precip Intensity" :is-int false :keep true  :unit "mm/h"}
	 :Wsymb    {:str "Symbol"                  :is-int true  :keep true  :unit "code"}})

(def image-dir "resources/")

(defn read-image
	[fname]
	(javax.imageio.ImageIO/read (java.io.File. (str (if (not (clojure.string/includes? fname "/")) image-dir) fname))))

(def map-pic   		(read-image "map3D.png"))
(def hour-hand 		(read-image "clock-hour.png"))
(def min-hand  		(read-image "clock-minute.png"))
(def sec-hand  		(read-image "clock-second.png"))
(def clock-pic 		(read-image "clock-rim.png"))
(def compass-pic 	(read-image "compass.png"))
(def arrow-pic 		(read-image "arrow.png"))
(def landscape-pic 	(atom nil))

(def symbol-pics   { 1 (read-image "symbol-01A.png")
					 2 (read-image "symbol-02A.png")
					 3 (read-image "symbol-03A.png")
					 4 (read-image "symbol-04A.png")
					 5 (read-image "symbol-05A.png")
					 6 (read-image "symbol-06A.png")
					 7 (read-image "symbol-07A.png")
					 8 (read-image "symbol-08A.png")
					 9 (read-image "symbol-09A.png")
					10 (read-image "symbol-10A.png")
					11 (read-image "symbol-11A.png")
					12 (read-image "symbol-12A.png")
					13 (read-image "symbol-13A.png")
					14 (read-image "symbol-14A.png")
					15 (read-image "symbol-15A.png")})

(def tiny-symbol-pics { 0 (read-image "symbol-00As.png")
						1 (read-image "symbol-01As.png")
					    2 (read-image "symbol-02As.png")
						3 (read-image "symbol-03As.png")
						4 (read-image "symbol-04As.png")
						5 (read-image "symbol-05As.png")
						6 (read-image "symbol-06As.png")
						7 (read-image "symbol-07As.png")
						8 (read-image "symbol-08As.png")
						9 (read-image "symbol-09As.png")
					   10 (read-image "symbol-10As.png")
					   11 (read-image "symbol-11As.png")
					   12 (read-image "symbol-12As.png")
					   13 (read-image "symbol-13As.png")
					   14 (read-image "symbol-14As.png")
					   15 (read-image "symbol-15As.png")})

(def res-scale       1)
(def horiz-res       (* 1920 res-scale))
(def vert-rez        (* 1080 res-scale))
(def graphics-height (* vert-rez 1/3))
(def graphics-width  horiz-res)
(def clock-height    (- vert-rez graphics-height))
(def clock-width     clock-height)
(def radar-height    (* clock-height 2/3))
(def radar-width     (- horiz-res clock-width))
(def info-height     (- clock-height radar-height))
(def info-width      radar-width)
(def wnow-height     (/ (- clock-height radar-height) 2))
(def wnow-width      (/ radar-width 5))

(def left-axis-width  100)
(def right-axis-width 50)
(def max-rain-level   5)
(def graph-days       7)
(def week-minutes     (* graph-days 24 60))
(def temp-padding     5)
(def wind-padding     2)
(def tick-width       10)
(def temp-text-x      (- left-axis-width tick-width 3))
(def wind-text-x      (- (/ left-axis-width 2) tick-width 3))
(def tot-temp-span    20)
(def axis-span        10)
(def rain-axis-span   max-rain-level)
(def wind-axis-factor 3)

(def radar-sub-width        112)
(def radar-sub-height       45)
(def radar-sub-upper-left-x 110)
(def radar-sub-upper-left-y 595)

(def axis-width     2)
(def wind-style     (style :foreground :white :background :lightgray))
(def rain-style     (style :foreground :blue  :background :blue))
(def temp-style     (style :foreground :red   :stroke 3))
(def axis-style     (style :foreground :white :stroke axis-width))
(def day-tick-style (style :foreground :grey :stroke 1))

(def axis-font-name       (str "ARIAL-" (int (* res-scale 20))))
(def axis-txt-font        (font axis-font-name))
(def temp-axis-text-style (style :foreground :red :font axis-font-name))
(def wind-axis-text-style (style :foreground :grey :font axis-font-name))
(def rain-axis-text-style (style :foreground :blue :font axis-font-name))
(def zero-line-style      (style :foreground :white :stroke 1))
(def lbl-txt-color        "#FFFFFF")
(def lbl-txt-font         (str "ARIAL-" (int (* res-scale 48))))
(def lbl-info-txt-font    (str "ARIAL-" (int (* res-scale 18))))
(def degree-char          "\u00b0")
(def text-circle-style    (style :foreground :white :background :white))
(def info-title-style     (style :foreground :white :font lbl-info-txt-font))
(def info-value-style     (style :foreground :white :font lbl-txt-font))
(def info-bg-style        (style :foreground (color 32 32 32)
								 :stroke 2
								 :background (color 128 128 128 128)))

; return current dat & time as a string
(defn now-str
	[]
	(f/unparse (f/formatters :mysql) (l/local-now)))

; the current weather forecast
(def weather-data (atom nil))

; the current radar image
(def radar-data (atom nil))

; read a directory
(defn get-dir-list
	[dir re]
	(filter #(re-find re %) (map str (file-seq (io/file dir)))))

; get list of available background images
(defn get-background-list
	[]
	(get-dir-list image-dir #"background-\d+\.(png|jpg|jpeg)$"))

; pick a random background
(defn get-background-name
	[]
	(let [backgrounds (get-background-list)
		  num-bg      (count backgrounds)]
		(nth backgrounds (rand-int num-bg))))

; load an image and set it as background
(defn set-background
	[]
	(let [bg-name  (get-background-name)
		  bg-image (read-image bg-name)]
		(swap! landscape-pic (fn [x] bg-image))))

; map wind direction angle to text
(defn wind-dir-to-str
	[dir]
	(let [between (fn [x [l h]] (and (>= x l) (< x h)))
		  wd [[[  0.0  22.5] "N"]
			  [[ 22.5  67.5] "NE"]
			  [[ 67.5 112.5] "E"]
			  [[112.5 157.5] "SE"]
			  [[157.5 202.5] "S"]
			  [[202.5 247.5] "SW"]
			  [[247.5 292.5] "W"]
			  [[292.5 337.5] "NW"]
			  [[337.5 360.0] "N"]]]
		(->> wd (filter #(between dir (first %))) first second)))

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

; send a weather forecast request to SMHI
(defn send-weather-request
	[config]
	(let [req-str (str (get config :weather-url)
					   "/api/category/" (get config :category)
					   "/version/" (get config :version)
					   "/geotype/point/lon/" (get config :longitude)
					   "/lat/" (get config :latitude)
					   "/data.json")]
		(with-open [rdr (io/reader req-str)]
			(json/read-str (clojure.string/join (line-seq rdr)) :key-fn keyword))))

; get latest radar image from SMHI
(defn get-radar-image
	[url]
	(let [img     (javax.imageio.ImageIO/read (io/as-url url))
		  imgtype (java.awt.image.BufferedImage/TYPE_INT_ARGB)
		  width   (.getWidth img)
		  height  (.getHeight img)
		  simg    (java.awt.image.BufferedImage. width height imgtype)
		  g2d     (.createGraphics simg)]
		(.drawImage g2d img 0 0 width height nil)
		(.dispose g2d)
		simg))

; fill a graphics context with color
(defn fill
	[^java.awt.Graphics2D g2d color width height]
	(do
    	(.setPaint g2d color)
        (.fillRect g2d 0 0 width height)
        g2d))

; draw a list of lines
(defn draw-line-seq
	[^java.awt.Graphics2D g2d points draw-style]
	(doseq [pair (partition 2 1 points)]
		(draw g2d
        	(line (-> pair first first)
        		  (-> pair first second)
        		  (-> pair second first)
        		  (-> pair second second))
        	draw-style)))

; draw a (possibly scaled) image within a widget
(defn draw-image
	[widget ^java.awt.Graphics2D g2d image match valign halign iname]
	{:pre [(some #{match}  '(:both :width :height :min :max))
		   (some #{valign} '(:top :center :bottom))
		   (some #{halign} '(:left :center :right))]}
	(try
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
            ;(println "w-w:" width "w-h:" height "i-w:" image-width "i-h:" image-height "m:" match "v:" valign "h:" halign "name:" iname)
            ;(println "ws1:" width-scale-1 "hs1:" height-scale-1 "ws2:" width-scale-2 "hs2:" height-scale-2 "nw:" new-width "nh:" new-height "wo:" width-offset "ho:" height-offset "\n")
            (push g2d
            	;(if (or (= iname "clock") (= iname "radar"))
            	;	(fill g2d java.awt.Color/RED width height))
            	(draw (scale g2d width-scale-2 height-scale-2)
            		  (image-shape (/ width-offset width-scale-2) (/ height-offset height-scale-2) image)
              		  nil)))
        (catch Exception e
    		(println e))))

; the window size map
(def lbl-map (atom nil))

; print changed window sizes for logging
(defn update-lbl-map
	[m k v]
	(swap! lbl-map (fn [x] (assoc m k v)))
	(println (format "%s New size: %-16s width: %4d height: %4d" (now-str) k (:width v) (:height v))))

; keep track of window sizes for logging
(defn lbl-info
	[object ^java.awt.Graphics2D g2d]
	(let [id     (id-of object)
		  width  (.getWidth object)
		  height (.getHeight object)]
		;(println id width height)
		(if (nil? @lbl-map)
			(update-lbl-map {} id {:width width :height height})
			(if (contains? @lbl-map id)
				(let [w (-> @lbl-map (get id) :width)
					  h (-> @lbl-map (get id) :height)]
					(if (not (and (= w width) (= h height)))
						(update-lbl-map @lbl-map id {:width width :height height})))
				(update-lbl-map @lbl-map id {:width width :height height})))))

; calculate all the values needed for drawing the temerature graph
(defn get-temp-scaling
	[data top bottom]
	(let [min-temp       (apply min (map second data))
		  max-temp       (apply max (map second data))
		  height         (inc (- bottom top))
		  real-temp-span (- max-temp min-temp)
		  half-diff      (/ (- tot-temp-span real-temp-span) 2)
		  start          (int (math/floor (- min-temp half-diff)))
		  info           {:min         min-temp                    ; lowest value
		 				  :max         max-temp                    ; highest value
		 				  :axis-scale  (/ tot-temp-span axis-span) ; integer size of steps
		 				  :axis-start  start                       ; what integer does the axis start on
		 				  :multiplier  1                           ; multiplier for data
		 				  :y-scale     (/ height tot-temp-span)        ; scale factor for window
		 				  :temp-span   real-temp-span              ; lowest to highest value
		 				  :min-padding (- min-temp start)}]        ; how much to pad values with
		;(println info)
		info))

; extract all the data for a specific parameter
(defn get-param
	[data param]
	(map #(vector (first %) (-> % second param)) data))

; convert hour to 0-360
(defn hour-to-angle
	[h]
	{:pre [(and (>= h 0) (< h 24))]}
	(* (/ (mod h 12) 12) 360))

; rotate an image around its center
(defn center-rotate
	[image angle]
	(let [width       (.getWidth image)
          height      (.getHeight image)
          neg         (fn [x] (- 0 x))
          buffer      (buffered-image width height)]
        (-> buffer
			.createGraphics
			;(fill java.awt.Color/RED width height)
			(translate (/ width 2) (/ height 2))
			(rotate angle)
			(translate (neg (/ width 2)) (neg (/ height 2)))
			(draw (image-shape 0 0 image) nil))
        buffer))

; draw the overall background
(defn draw-background
	[widget ^java.awt.Graphics2D g2d]
	(try
		(lbl-info widget g2d)
		(if (not (nil? @landscape-pic))
			(draw-image widget g2d @landscape-pic :both :center :center "background"))
		(catch Exception e
    		(println e))))

; draw the radar picture
(defn draw-radar
	[widget ^java.awt.Graphics2D g2d]
	(try
		(lbl-info widget g2d)
		(if (not (nil? @radar-data))
	    	(let [width        (.getWidth widget)
	        	  height       (.getHeight widget)
	        	  map-width    (.getWidth map-pic)
	        	  map-height   (.getHeight map-pic)
	        	  mid-x        (/ (- width map-width) 2)
	              mid-y        (/ (- height map-height) 2)
	              border-size  10
	              ;a            (fill (.createGraphics @radar-data) (color 128 128 128) (.getWidth @radar-data) (.getHeight @radar-data))
	              sub-radar    (.getSubimage @radar-data
	              							 radar-sub-upper-left-x 
	              							 radar-sub-upper-left-y 
	              							 radar-sub-width 
	              							 radar-sub-height)
	              width-ratio  (/ (- map-width (* border-size 2)) radar-sub-width)
	              height-ratio (/ (- map-height (* border-size 2)) radar-sub-height)
	              buffer       (buffered-image map-width map-height)
	          	  buffer-g2d   (.createGraphics buffer)
	          	  aaa          (-> buffer-g2d (draw (image-shape 0 0 map-pic) nil)
	          	  							  (scale width-ratio height-ratio)
	          	  							  (draw (image-shape 1 1 sub-radar) nil))]
	            ;(println "lw: " width " lh: " height " mw: " map-width " mh: " map-height)
	            ;(println "rw:" radar-sub-width "rh:" radar-sub-height "wr:" width-ratio "hr:" height-ratio)
	            ;(println "ofs:" (/ border-size width-ratio))
	            (draw-image widget g2d buffer :min :center :center "radar")))
        (catch Exception e
    		(println e))))

; draw the clock
(defn draw-clock
	[widget ^java.awt.Graphics2D g2d]
	(try
		(lbl-info widget g2d)
    	(let [now          (l/local-now)
              minute       (+ (t/minute now) (/ (t/second now) 60))
              hour         (+ (t/hour now) (/ minute 60))
              rotated-hour (center-rotate hour-hand (hour-to-angle hour))
              rotated-min  (center-rotate min-hand (* minute 6))
              rotated-sec  (center-rotate sec-hand (* (t/second now) 6))]
            ;(println "min:" (double minute) "hour:" (double hour) "hd:" (double (* hour 6)) "md:" (double (* minute 6)))
            (draw-image widget g2d clock-pic    :min :center :center "clock")
            (draw-image widget g2d rotated-hour :min :center :center "hour")
        	(draw-image widget g2d rotated-min  :min :center :center "minute")
        	(draw-image widget g2d rotated-sec  :min :center :center "second"))
        (catch Exception e
    		(println e))))

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
        	rain-style)))

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
		;(println w-gust)
		;(println "max-gust: " max-gust " y-scale: " y-scale " height: " height)
		;(println g-points)
		(draw g2d
        	(apply polygon (concat g-points s-points))
        	wind-style)))

; calculate width of string (in pixels)
(defn string-width
	[^java.awt.Graphics2D g2d txt-style txt]
	;(println "string-width:" (type (:font txt-style)) (:font txt-style))
	(let [font-metrics (.getFontMetrics g2d (:font txt-style))
		  txt-width    (.stringWidth font-metrics txt)]
		txt-width))

; calculate height of string (in pixels)
(defn string-height
	[^java.awt.Graphics2D g2d txt-style]
	(let [font-metrics (.getFontMetrics g2d (:font txt-style))
		  txt-height   (.getHeight font-metrics)]
		txt-height))

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
					   true)
		)
	))

(defn avg
	[coll]
	(if (empty? coll)
		0
		(int (/ (apply + coll) (count coll)))))

; draw the weather symbols on the forecast
(defn draw-graph-symbols
	[^java.awt.Graphics2D g2d data top width]
	(let [symb-data    (get-param data :Wsymb)
		  group-length (/ week-minutes graph-days 2)
		  group-width  (/ width (* graph-days 2))
		  group-offset (- (/ group-width 2) (/ (.getWidth (get tiny-symbol-pics 0)) 2))
		  get-data     (fn [coll] (map second coll))
		  groups       (for [minutes (range 0 week-minutes group-length)]
		  			   	   (filter #(and (>= (first %) minutes) (< (first %) (+ minutes group-length)))
		  			   	   		   symb-data))
		  grp-avg      (map #(avg (get-data %)) groups)]
		(doseq [i (range (* graph-days 2))]
			(draw g2d
				  (image-shape (+ left-axis-width (* i group-width) group-offset)
				  			   top
				  			   (get tiny-symbol-pics (nth grp-avg i)))
				  nil))))

; draw the forecast graphics
(defn draw-curve
	[widget ^java.awt.Graphics2D g2d]
	(try
		(lbl-info widget g2d)
		(if (not (nil? @weather-data))
	    	(let [width        (.getWidth widget)
	        	  height       (.getHeight widget)
	        	  top          axis-width
	        	  bottom       (- height axis-width)
	        	  graph-height (- bottom top)
	        	  width-avail  (- width left-axis-width right-axis-width)
	        	  x-scale      (/ width-avail week-minutes)
	        	  x-data       (map #(vector (-> % first (* x-scale) (+ left-axis-width) int) (second %)) @weather-data)
	        	  temp-data    (map #(vector (first %) (->> % second (* 1.0))) (get-param x-data :t))
			  	  temp-info    (get-temp-scaling temp-data top bottom)]
			  	(fill g2d (color 128 128 128 128) width height)
	        	(draw-axis g2d x-data width top bottom temp-info)
	        	(draw-rain g2d x-data top bottom)
	        	(draw-wind g2d x-data top bottom)
	        	(draw-temp g2d temp-data temp-info top bottom)
	        	(draw-graph-symbols g2d @weather-data top width-avail)))
    (catch Exception e
      (println e))))

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
      (println e))))

; draw the symbol for current weather
(defn draw-w-symbol
	[widget ^java.awt.Graphics2D g2d]
	(try
		(lbl-info widget g2d)
		(if (not (nil? @weather-data))
	    	(let [symb-num (-> @weather-data first second :Wsymb)]
				(draw-image widget g2d (get symbol-pics symb-num) :min :center :center "symbol")))
    (catch Exception e
      (println e))))

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
			  value-style)
		))

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
				(dit 1 1 "Rain mm/h"      (wind-dir-to-str (v-frmt :pmedian)))
				(dit 2 1 "Thunder"        (str (v-frmt :tstm) "%"))
				(dit 3 1 "Wind Dir"       (str (v-frmt :wd)))
			))
		(catch Exception e (println e))))

; create the frame
(def smhi-frame
	(window
		:width horiz-res
		:height vert-rez
		:content
			(xyz-panel :items [
				(label	:id 		:lbl-symbol
						:bounds		[(* wnow-width 4) (* wnow-height 0) wnow-width wnow-height]
						:foreground lbl-txt-color 
						:paint 		draw-w-symbol)
				(label	:id 		:wind-dir
						:bounds		[(* wnow-width 4) (* wnow-height 1) wnow-width wnow-height]
						:foreground lbl-txt-color 
						:paint 		draw-wind-dir)

				(label	:id 		:info  ; info
						:bounds		[0 0 info-width info-height]
						:paint 		draw-info)
				(label	:id 		:radar  ; radar
						:bounds		[0 info-height radar-width radar-height]
						:paint 		draw-radar)
				(label	:id 		:clock  ; the clock
						:bounds		[radar-width 0 clock-width clock-height]
						:listen     [:mouse-clicked (fn [e] (java.lang.System/exit 0))]
						:paint 		draw-clock)
				(label	:id 		:forecast  ; forecast graphics
						:bounds		[0 clock-height graphics-width graphics-height]
						:paint 		draw-curve)
				(label 	:id 		:lbl-back ; background
						:bounds		[0 0 horiz-res vert-rez]
						:paint 		draw-background)
			])))

; convert the SMHI timestamp to minutes from midnight (or -1 if it's too old)
(defn mk-delta-time
	[m now day-start]
	(let [map-time    (f/parse (:validTime m))
		  map-minutes (if (t/before? map-time now)
						  -1
						  (t/in-minutes (t/interval day-start map-time)))]
		map-minutes))

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
					   		  						 (get (get units (first map-keys)) :is-int)))))
		)))

; process the SMHI data and convert timestamp to delta minutes
(defn process-data
	[]
	(let [now  		(t/now)
  		  day-start (t/today-at 00 00)
  		  resp  	(:timeSeries (send-weather-request smhi-config))
  		  filtered-resp (map #(vector (mk-delta-time % now day-start)
  		  							  (filter-param-map (:parameters %) units))
  		  							  resp)
  		  valid-resp	(filter #(>= (first %) 0) filtered-resp) ; remove old ones
  		  week-resp		(filter #(< (first %) week-minutes) valid-resp) ; only this week
  		  ]
  		(if (> (count valid-resp) (count week-resp))
  			(let [next-entry (nth valid-resp (count week-resp))
  				  prev-entry (last week-resp)
  				  new-entry  (mk-intermediate prev-entry next-entry (dec week-minutes))]
  				(conj (vec week-resp) new-entry))
  			week-resp)
  		))

(defn weather-timer-fn
	[x]
	(try
		(let [weather (process-data)]
			(println (now-str) "weather-timer-fn: successfully got new forecast")
			(swap! weather-data (fn [x] weather))
			(if (not (nil? @weather-data))
				(repaint! [(select smhi-frame [:#info])
			    		   (select smhi-frame [:#wind-dir])
			    		   (select smhi-frame [:#lbl-symbol])
						   (select smhi-frame [:#forecast])])))
		(catch Exception e
    		(println (now-str) "weather-timer-fn exception: " e))))

(defn radar-timer-fn
	[x]
	(try
		(let [pic (get-radar-image (:radar-url smhi-config))]
			(println (now-str) "radar-timer-fn: successfully got new image")
			(swap! radar-data (fn [x] pic))
			(repaint! (select smhi-frame [:#radar]))
			(set-background)
			(repaint! (select smhi-frame [:#lbl-back])))
		(catch Exception e
    		(println (now-str) "radar-timer-fn exception: " e))))

(defn -main
  	[& args]
  	(set-background)
	(-> smhi-frame show!) ; full-screen!)
	(st/timer (fn [x] (repaint! (select smhi-frame [:#clock]))))
	(st/timer weather-timer-fn 
			  :initial-delay (* 1000 2) 
			  :delay (* 1000 60 30))
	(st/timer radar-timer-fn 
			  :delay (* 1000 60 5))
	;(.setUndecorated smhi-frame true)
	;(.setFullScreenWindow (.getDefaultScreenDevice (GraphicsEnvironment/getLocalGraphicsEnvironment)) smhi-frame)
)
