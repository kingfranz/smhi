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
    (:require [smhi.utils                 :as utils]
              [smhi.graph-utils           :as gutils]
              [smhi.spec                  :as spec]
              [smhi.config                :as conf]
              [smhi.sun                   :as sun]
              [clojure.data.json          :as json]
              [clojure.java.io            :as io]
              [clojure.spec               :as s]
              [clj-time.core              :as t]
              [clj-time.format            :as f]
              [clj-time.local             :as l]
              [clojure.math.numeric-tower :as math]
             [clojure.core.async   :as async :only [go]]
             [async-watch.core     :as aw]
              [seesaw.timer               :as st]
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
    []
    (str (get @conf/config :weather-url)
     "/api/category/"      (get @conf/config :category)
     "/version/"           (get @conf/config :version)
     "/geotype/point/lon/" (get @conf/config :longitude)
     "/lat/"               (get @conf/config :latitude)
     "/data.json"))

; send a weather forecast request to SMHI
(defn send-weather-request
    []
    (let [forecast (utils/send-json-request (mk-weather-url))]
        (if (= (s/conform spec/smhi-spec forecast) :clojure.spec/invalid)
            (do
                (timbre/error "------- Invalid SMHI data -----------")
                (timbre/error (s/explain-str spec/smhi-spec forecast))
                (timbre/error "-------------------------------------")
                (timbre/error forecast)
                (throw (Exception. "Invalid SMHI data"))))
        forecast))

; get latest radar image from SMHI
(defn get-radar-image
    []
    (let [data    (utils/send-request (:radar-url @conf/config) :image)
          img     (utils/byte-array-2-image data)
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
        (sg/push g2d
            (sg/draw (sg/scale g2d width-scale-2 height-scale-2)
                (sg/image-shape (/ width-offset width-scale-2) (/ height-offset height-scale-2) image)
                nil))))

; calculate all the values needed for drawing the temerature graph
(defn get-temp-scaling
    [data top bottom]
    (let [min-temp       (if conf/fixed-temp (:min-fixed-temp @conf/config) (apply min (map second data)))
          max-temp       (if conf/fixed-temp (:max-fixed-temp @conf/config) (apply max (map second data)))
          height         (inc (- bottom top))
          real-temp-span (- max-temp min-temp)
          half-diff      (/ (- (conf/tot-temp-span) real-temp-span) 2)
          start          (int (math/floor (- min-temp half-diff)))]
        {:min         min-temp                    ; lowest value
         :max         max-temp                    ; highest value
         :axis-scale  (/ (conf/tot-temp-span) conf/axis-span) ; integer size of steps
         :axis-start  start                       ; what integer does the axis start on
         :multiplier  1                           ; multiplier for data
         :y-scale     (/ height (conf/tot-temp-span))        ; scale factor for window
         :temp-span   real-temp-span              ; lowest to highest value
         :min-padding (- min-temp start)}))        ; how much to pad values with

; extract all the data for a specific parameter
(defn get-param
  [data param]
  (map #(vector (first %) (-> % second param)) data))

; draw the overall background
(defn draw-background
    [widget ^java.awt.Graphics2D g2d]
    (try
        (utils/lbl-info widget g2d)
        (if-not (nil? @gutils/landscape-pic)
            (draw-image widget g2d @gutils/landscape-pic :both :center :center "background"))
        (catch Exception e
            (timbre/error (Exception. e)))))

(defn draw-exception-txt
    [widget ^java.awt.Graphics2D g2d e]
    (let [txt      (.getMessage e)
          width    (.getWidth widget)
          height   (.getHeight widget)
          t-width  (utils/string-width  g2d conf/exception-style txt)
          t-height (utils/string-height g2d conf/exception-style)]
        (sg/draw g2d
            (sg/string-shape (/ (- width t-width) 2)
                          (/ (- height t-height) 2)
                          txt)
            conf/exception-style)))

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
          buffer       (sg/buffered-image map-width map-height)
          buffer-g2d   (.createGraphics buffer)
          time-txt     (f/unparse (f/with-zone (f/formatters :hour-minute) (t/default-time-zone)) (l/local-now))
          time-width   (utils/string-width buffer-g2d conf/radar-txt-style time-txt)
          time-height  (utils/string-height buffer-g2d conf/radar-txt-style)]
        (-> buffer-g2d (sg/draw (sg/image-shape 0 0 map-pic) nil)
                       (sg/scale width-ratio height-ratio)
                       (sg/draw (sg/image-shape 1 1 sub-radar) nil))
        (sg/draw (.createGraphics buffer) (sg/string-shape (- 230 time-width) time-height time-txt) conf/radar-txt-style)
        buffer))

(defn have-radar-data
    []
    (if (and (some? @radar-data) (not-empty @radar-data))
        true
        (do
            (when-let [queue @radar-queue]
                (reset! radar-data (apply conj queue (repeat (* (:radar-fps @conf/config) (:radar-ani-delay-sec @conf/config)) (last queue)))))
            (and (some? @radar-data) (not-empty @radar-data)))))

(defn get-current-radar-image
    []
    (if (have-radar-data)
        (let [image (peek @radar-data)]
            (reset! radar-data (pop @radar-data))
            image)
        nil))

(defn add-radar-2-queue
    [image]
    (if (nil? @radar-queue)
        (reset! radar-queue (clojure.lang.PersistentQueue/EMPTY)))
    (if (> (count @radar-queue) (conf/max-radar-queue-size))
        (reset! radar-queue (pop @radar-queue)))
    (reset! radar-queue (conj @radar-queue image)))

; draw the radar picture
(defn draw-radar
  [widget ^java.awt.Graphics2D g2d]
  (try
    (utils/lbl-info widget g2d)
    (when-let [image (get-current-radar-image)]
        (sg/draw g2d (sg/image-shape 0 0 image) nil))
    (if (not (nil? @radar-exception))
        (draw-exception-txt widget g2d @radar-exception))
    (catch Exception e
        (timbre/error (Exception. e)))))

; draw the clock
(defn draw-clock
  [widget ^java.awt.Graphics2D g2d]
  (try
    (utils/lbl-info widget g2d)
    ;(info "tick")
    (let [now          (l/local-now)
            minute       (+ (t/minute now) (/ (t/second now) 60))
            hour         (+ (t/hour now) (/ minute 60))
            rotated-hour (gutils/center-rotate (:hour-hand @utils/clock-pics) (utils/hour-to-angle hour))
            rotated-min  (gutils/center-rotate (:min-hand @utils/clock-pics) (* minute 6))
            rotated-sec  (gutils/center-rotate (:sec-hand @utils/clock-pics) (* (t/second now) 6))]
        (draw-image widget g2d (:clock-pic @utils/clock-pics)    :min :center :center "clock")
        (draw-image widget g2d rotated-hour :min :center :center "hour")
        (draw-image widget g2d rotated-min  :min :center :center "minute")
        (draw-image widget g2d rotated-sec  :min :center :center "second"))
    (catch Exception e
     (timbre/error (Exception. e)))))

; draw graphic forecast of rain
(defn draw-rain
  [^java.awt.Graphics2D g2d data top bottom]
  (let [rain-data  (get-param data :pmedian)
        height     (inc (- bottom top))
        y-scale    (/ height (:max-rain-level @conf/config))
        points     (map #(vector (first %) (- bottom (-> % second (* y-scale)))) rain-data)
        all-points (concat [[(-> points first first) bottom]]
                    points
                    [[(-> points last first) bottom]])]
    (sg/draw g2d
          (apply sg/polygon all-points)
          (:rain-style @conf/config))
    (gutils/draw-line-seq g2d points (sg/style :foreground :white))))

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
    (gutils/draw-line-seq g2d points (:temp-style @conf/config))))

; draw graphic forecast of wind
(defn draw-wind
  [^java.awt.Graphics2D g2d data top bottom]
  (let [w-speed   (get-param data :ws)
        w-gust    (get-param data :gust)
        height    (inc (- bottom top))
        max-gust  (+ (apply max (map second w-gust)) conf/wind-padding)
        y-scale   (/ height (* conf/axis-span conf/wind-axis-factor))
        g-points  (map #(vector (first %) (- bottom (-> % second (* y-scale)))) w-gust)
        s-points  (map #(vector (first %) (- bottom (-> % second (* y-scale)))) (reverse w-speed))]
    (sg/draw g2d
          (apply sg/polygon (concat g-points s-points))
          (:wind-style @conf/config))))

; draw text with a circle background
(defn draw-text
  [^java.awt.Graphics2D g2d x y txt txt-style left-side]
  (let [txt-width    (utils/string-width g2d txt-style txt)
        txt-height   (utils/string-height g2d txt-style)
        radius       (+ (/ txt-height 2) 4)
        circle-x     (if left-side (- x (/ radius 2) 5) (+ x (/ radius 2) 5))
        txt-y        (+ y (/ txt-height 4))
        txt-x        (- circle-x (/ txt-width 2))]
    (sg/draw g2d
        (sg/circle circle-x y radius)
        (:text-circle-style @conf/config))
    (sg/draw g2d
        (sg/string-shape txt-x txt-y txt)
        (:txt-style @conf/config))))

; draw axises for forecast graphics (and scales)
(defn draw-axis
  [^java.awt.Graphics2D g2d data width top bottom temp-info]
  (let [left-x       (dec conf/left-axis-width)
        right-x      (- width conf/right-axis-width)
        graph-height (inc (- bottom top))
        day-width    (/ (- right-x left-x) (:graph-days @conf/config))
        rain-text-x  (+ (- width conf/right-axis-width) conf/tick-width 5)
        temp-start   (:axis-start temp-info)
        temp-scale   (:axis-scale temp-info)
        height-scale (/ graph-height conf/axis-span)
        font-metrics (.getFontMetrics g2d (:axis-txt-font @conf/config))]
    ; left vertical line
    (sg/draw g2d
      (sg/line left-x top
          left-x bottom)
      (:axis-style @conf/config))
    ; left left vertical line
    (sg/draw g2d
      (sg/line (/ left-x 2) top
          (/ left-x 2) bottom)
      (:axis-style @conf/config))
    ; right vertical line
    (sg/draw g2d
      (sg/line right-x top
          right-x bottom)
      (:axis-style @conf/config))
    ; bottom line
    (sg/draw g2d
      (sg/line left-x bottom
          right-x bottom)
      (:axis-style @conf/config))
    ; zero line
    (if (and (< temp-start 0) (> temp-start (- 0 (conf/tot-temp-span))))
      (sg/draw g2d
        (sg/line left-x  (- bottom (* (- 0 temp-start) (/ graph-height (conf/tot-temp-span))))
              right-x (- bottom (* (- 0 temp-start) (/ graph-height (conf/tot-temp-span)))))
        (:zero-line-style @conf/config)))
    ; day lines
    (doseq [day-line (range 1 (:graph-days @conf/config))]
      (sg/draw g2d
        (sg/line (+ left-x (* day-line day-width)) top
            (+ left-x (* day-line day-width)) bottom)
        (:day-tick-style @conf/config)))
    ; ticks on axises
    (doseq [tick-idx (range conf/axis-span)]
      ; temp axis
      (sg/draw g2d
        (sg/line left-x
            (- bottom (* tick-idx height-scale))
            (- left-x (if (even? tick-idx) conf/tick-width (/ conf/tick-width 2)))
            (- bottom (* tick-idx height-scale)))
        (:axis-style @conf/config))
      ; wind axis
      (sg/draw g2d
        (sg/line right-x
            (- bottom (* tick-idx height-scale))
            (+ right-x (if (even? tick-idx) conf/tick-width (/ conf/tick-width 2)))
            (- bottom (* tick-idx height-scale)))
        (:axis-style @conf/config))
      ; rain axis
      (sg/draw g2d
        (sg/line (/ left-x 2)
            (- bottom (* tick-idx height-scale))
            (- (/ left-x 2) (if (even? tick-idx) conf/tick-width (/ conf/tick-width 2)))
            (- bottom (* tick-idx height-scale)))
        (:axis-style @conf/config)))
    ; axis text
    (doseq [text-idx (range 2 conf/axis-span 2)]
      (draw-text g2d
             (- left-x conf/tick-width)
             (- bottom (* text-idx height-scale))
             (format "%d" (+ temp-start (* temp-scale text-idx)))
             (:temp-axis-text-style @conf/config)
             true)
      (draw-text g2d
             (+ right-x conf/tick-width)
             (- bottom (* text-idx height-scale))
             (format "%d" (* text-idx conf/wind-axis-factor))
             (:wind-axis-text-style @conf/config)
             false)
      (draw-text g2d
             (- (/ left-x 2) conf/tick-width)
             (- bottom (* text-idx height-scale))
             (format "%d" (/ text-idx (/ conf/axis-span (conf/rain-axis-span))))
             (:rain-axis-text-style @conf/config)
             true))))

; draw axises for forecast graphics (and scales)
(defn draw-sunrise
    [^java.awt.Graphics2D g2d left-x left-y width height]
    (try
        (when-let [info (sun/get-sun-info)]
            (info "drawing sunrise and sunset")
            (let [sec-2-x              (fn [s] (* (/ width (* (:graph-days @conf/config) 24 60 60)) s))
                  dt-to-sec            (fn [x] (+ (* (t/hour x) 60 60) (* (t/minute x) 60) (t/second x)))
                  twilight-start-sec   (dt-to-sec (->> info :results conf/twilight-begin f/parse))
                  sunrise-sec          (dt-to-sec (->> info :results :sunrise f/parse))
                  sunset-sec           (dt-to-sec (->> info :results :sunset f/parse))
                  twilight-end-sec     (dt-to-sec (->> info :results conf/twilight-end f/parse))
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
                  mk-gradient          (fn [xs w l] (sg/style :background
                                                    (sg/linear-gradient :start [xs 0]
                                                                     :end [(+ xs w) 0]
                                                                     :colors [(sclr/color 30 30 30 (if l 255 0))
                                                                              (sclr/color 30 30 30 (if l 0 255))])))

                  draw-morning-night (fn [d] (sg/draw g2d (sg/rect (day-start d)
                                                             left-y
                                                             night-start-width
                                                             height)
                                                    (sg/style :background (sclr/color 30 30 30))))
                  draw-morning-twilight (fn [d] (sg/draw g2d
                                                      (sg/rect (twilight-start d)
                                                            left-y
                                                            twilight-start-width
                                                            height)
                                                      (mk-gradient (twilight-start d) twilight-start-width true)))
                  draw-evening-twilight (fn [d] (sg/draw g2d
                                                      (sg/rect (sunset-start d)
                                                            left-y
                                                            twilight-end-width
                                                            height)
                                                      (mk-gradient (sunset-start d) twilight-end-width false)))
                  draw-evening-night (fn [d] (sg/draw g2d (sg/rect (night-start d)
                                                             left-y
                                                             night-end-width
                                                             height)
                                                       (sg/style :background (sclr/color 30 30 30))))]
                (doseq [d (range (:graph-days @conf/config))]
                    (draw-morning-night d)
                    (draw-morning-twilight d)
                    (draw-evening-twilight d)
                    (draw-evening-night d))))
        (catch Exception e
            (timbre/error (Exception. "draw-sunrise")))))
    
(defn mk-symbol-targets
  []
  (flatten (for [d    (range 0 (:graph-days @conf/config))
                 :let [d24 (* d 24 60)
                        x   [(+ d24 (* 6 60)) (+ d24 (* 12 60)) (+ d24 (* 18 60))]]]
            x)))

(defn pick-closer
  [[x1 y1] t [x2 y2]]
  (let [dist1 (utils/abs (- t x1))
        dist2 (utils/abs (- t x2))]
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
        day-width     (/ width (:graph-days @conf/config))
        symbol-width  (.getWidth (get @utils/tiny-symbol-pics 0))
        symbol-offset (/ (- day-width (* symbol-width symbs-per-day)) (inc symbs-per-day))]
    (doseq [day (range (:graph-days @conf/config))]
      (doseq [symb-idx (range symbs-per-day)]
        (let [x   (+ left-side (* day-width day) symbol-offset (* symb-idx (+ symbol-width symbol-offset)))
              y   top
              ii  (nth symb-values (+ (* day symbs-per-day) symb-idx))
              img (get @utils/tiny-symbol-pics ii)]
          (sg/draw g2d (sg/image-shape x top img) nil))))))

(defn draw-dates
  [^java.awt.Graphics2D g2d top height left-side width]
  (let [date-strings (utils/mk-date-strings)
        day-width    (/ width (:graph-days @conf/config))]
    (doseq [date-idx (range (:graph-days @conf/config))
            :let [dstr-width (utils/string-width g2d conf/date-txt-style (nth date-strings date-idx))
                  dstr-height (utils/string-height g2d conf/date-txt-style)]]
      (sg/draw g2d
          (sg/string-shape (- (+ left-side (/ day-width 2) (* day-width date-idx)) (/ dstr-width 2))
                  (+ top (/ dstr-height 2) (/ height 2) -3)
                  (nth date-strings date-idx))
          conf/date-txt-style))))

; draw the forecast graphics
(defn draw-curve
  [widget ^java.awt.Graphics2D g2d]
  (try
    (utils/lbl-info widget g2d)
    (if (not (nil? @weather-data))
        (let [width        (.getWidth widget)
              height       (.getHeight widget)
              date-height  30
              top          conf/axis-width
              bottom       (- height date-height conf/axis-width)
              graph-height (- bottom top)
              width-avail  (- width conf/left-axis-width conf/right-axis-width)
              x-scale      (/ width-avail (conf/week-minutes))
              x-data       (map #(vector (-> % first (* x-scale) (+ conf/left-axis-width) int) (second %)) @weather-data)
              temp-data    (map #(vector (first %) (->> % second (* 1.0))) (get-param x-data :t))
              temp-info    (get-temp-scaling temp-data top bottom)]
          (gutils/fill g2d (sclr/color 128 128 128 128) width height)
          (draw-sunrise g2d conf/left-axis-width top width-avail (inc (- bottom top)))
          (draw-axis g2d x-data width top bottom temp-info)
          (draw-rain g2d x-data top (- bottom 3))
          (draw-wind g2d x-data top bottom)
          (draw-temp g2d temp-data temp-info top bottom)
          (draw-graph-symbols g2d @weather-data top width-avail conf/left-axis-width)
          (draw-dates g2d (- height date-height) date-height conf/left-axis-width width-avail)))
    (if (not (nil? @weather-exception))
        (draw-exception-txt widget g2d @weather-exception))
    (catch Exception e
      (timbre/info (Exception. "draw-curve") "arg1" "arg2"))))

; draw the compass and wind direction
(defn draw-wind-dir
  [widget ^java.awt.Graphics2D g2d]
  (try
    (utils/lbl-info widget g2d)
    (if (not (nil? @weather-data))
        (let [direction      (-> @weather-data first second :wd)
                rotated-arrow  (gutils/center-rotate (:arrow-pic @utils/clock-pics) (mod (+ direction 180) 360))]
         (draw-image widget g2d (:compass-pic @utils/clock-pics) :min :center :center "compass")
         (draw-image widget g2d rotated-arrow :min :center :center "arrow")))
    (catch Exception e
      (timbre/error (Exception. e)))))

; draw the symbol for current weather
(defn draw-w-symbol
  [widget ^java.awt.Graphics2D g2d]
  (try
    (utils/lbl-info widget g2d)
    (if (not (nil? @weather-data))
        (let [symb-num (-> @weather-data first second :Wsymb)]
         (draw-image widget g2d (get @utils/symbol-pics symb-num) :min :center :center "symbol")))
    (catch Exception e
      (timbre/error (Exception. e)))))

; draw background, title and value for one section of now info
(defn draw-info-text
  [^java.awt.Graphics2D g2d center-x width top height title title-style value value-style]
  (let [t-width       (utils/string-width  g2d title-style title)
        t-height      (utils/string-height g2d title-style)
        v-width       (utils/string-width  g2d value-style (str value))
        v-height      (utils/string-height g2d value-style)
        top-border    9
        center-border 6
        bottom-border 14
        side-border   10
        title-part    1/3
        value-part    2/3
        title-height  (* height title-part)
        value-height  (* height value-part)]
    ; draw top sq
    (sg/draw g2d
      (sg/rounded-rect (+ (- center-x (/ width 2)) side-border)
              (+ top top-border)
              (- width (* side-border 2))
              (- title-height top-border center-border)
              10 10)
      conf/info-bg-style)
    ; draw bottom sq
    (sg/draw g2d
      (sg/rounded-rect (+ (- center-x (/ width 2)) side-border)
              (+ top title-height)
              (- width (* side-border 2))
              (- value-height bottom-border)
              10 10)
      conf/info-bg-style)
    ; draw top txt
    (sg/draw g2d
        (sg/string-shape (- center-x (/ t-width 2))
                (+ top (/ title-height 2) (/ t-height 2) -3)
                title)
        title-style)
    ; draw bottom txt
    (sg/draw g2d
        (sg/string-shape (- center-x (/ v-width 2))
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
                                          conf/info-title-style
                                          v
                                          conf/info-value-style))
              cloud-cover  (-> (v-frmt :tcc_mean) (/ 8) (* 100) int)]
        (dit 0 0 "Temp"           (str (v-frmt :t) conf/degree-char))
        (dit 1 0 "Humidity"       (str (v-frmt :r) "%"))
        (dit 2 0 "Cloud"          (str cloud-cover "%"))
        (dit 3 0 "Wind Speed m/s" (str (v-frmt :ws) "-" (v-frmt :gust)))
        (dit 0 1 "Pressure"       (str (v-frmt :msl)))
        (dit 1 1 "Rain mm/h"      (str (v-frmt :pmedian)))
        (dit 2 1 "Thunder"        (str (v-frmt :tstm) "%"))
        (dit 3 1 "Wind Dir"       (utils/wind-dir-to-str (v-frmt :wd)))))
    (catch Exception e
      (timbre/error (Exception. e)))))

; create the frame
(def smhi-frame
  (sc/window
    :width conf/horiz-res
    :height conf/vert-rez
    :content
      (sc/xyz-panel :items [
                         (sc/label  :id     :lbl-symbol
                             :bounds    [(* conf/wnow-width 4) (* conf/wnow-height 0) conf/wnow-width conf/wnow-height]
                             :foreground conf/lbl-txt-color
                             :paint     draw-w-symbol)
                         (sc/label  :id     :wind-dir
                             :bounds    [(* conf/wnow-width 4) (* conf/wnow-height 1) conf/wnow-width conf/wnow-height]
                             :foreground conf/lbl-txt-color
                             :paint     draw-wind-dir)

                         (sc/label  :id     :info  ; info
                             :bounds    [0 0 conf/info-width conf/info-height]
                             :paint     draw-info)
                         (sc/label  :id     :radar  ; radar
                             :bounds    [0 conf/info-height conf/radar-width conf/radar-height]
                             :paint     draw-radar)
                         (sc/label  :id     :clock  ; the clock
                             :bounds    [conf/radar-width 0 conf/clock-width conf/clock-height]
                             :listen     [:mouse-clicked (fn [e] (java.lang.System/exit 0))]
                             :paint     draw-clock)
                         (sc/label  :id     :forecast  ; forecast graphics
                             :bounds    [0 conf/clock-height conf/graphics-width conf/graphics-height]
                             :paint     draw-curve)
                         (sc/label   :id     :lbl-back ; background
                             :bounds    [0 0 conf/horiz-res conf/vert-rez]
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
                                (get (get conf/units (first map-keys)) :is-int))))))))

; process the SMHI data and convert timestamp to delta minutes
(defn process-data
    []
    (let [now           (t/now)
          day-start     (t/today-at 00 00)
          response      (send-weather-request)
          resp          (:timeSeries response)
          filtered-resp (map #(vector (mk-delta-time % now day-start)
                                      (filter-param-map (:parameters %) conf/units))
                             resp)
          valid-resp    (filter #(>= (first %) 0) filtered-resp) ; remove old ones
          week-resp     (filter #(< (first %) (conf/week-minutes)) valid-resp)] ; only this week
        (if (> (count valid-resp) (count week-resp))
            (let [next-entry (nth valid-resp (count week-resp))
                  prev-entry (last week-resp)
                  new-entry  (mk-intermediate prev-entry next-entry (dec (conf/week-minutes)))]
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
                (timbre/info "getting new forecast")
                (let [weather (process-data)]
                    (timbre/info "weather-timer-fn: successfully got new forecast")
                    (reset! weather-data weather)
                    (reset! weather-exception nil)
                    (reset! weather-timer-ts (l/local-now))
                    (if (not (nil? @weather-data))
                        (sc/repaint! [(sc/select smhi-frame [:#info])
                                      (sc/select smhi-frame [:#wind-dir])
                                      (sc/select smhi-frame [:#lbl-symbol])
                                      (sc/select smhi-frame [:#forecast])])))))
        (catch Exception e
            (do
                (reset! weather-exception e)
                (reset! weather-timer-ts nil)
                (timbre/error "---------------------------------------------------")
                (timbre/error "timbre/error in: weather-timer-fn")
                (timbre/error (str "Exception: " (.getMessage e)))
                (timbre/error "---------------------------------------------------")
                (spit "clock-timbre/error.log" (str (utils/now-str) " weather-timer-fn Exception: " (.getMessage e) "\n") :append true)
                (timbre/error (Exception. e))))))

(defn radar-timer-fn
    [x]
    (try
        (timbre/info "getting new radar image")
        (let [radar-pic (get-radar-image)
              combo-pic (mk-radar-image radar-pic
                                        (:map-pic @utils/clock-pics)
                                        (:radar-sub-upper-left-x @conf/config)
                                        (:radar-sub-upper-left-y @conf/config)
                                        (:radar-sub-width @conf/config)
                                        (:radar-sub-height @conf/config))]
            (timbre/info "radar-timer-fn: successfully got new image")
            (add-radar-2-queue combo-pic)
            (reset! radar-exception nil)
            (gutils/set-background)
            (sc/repaint! (sc/select smhi-frame [:#lbl-back])))
        (catch Exception e
            (do
                (reset! radar-exception e)
                (timbre/error "---------------------------------------------------")
                (timbre/error "error in: radar-timer-fn")
                (timbre/error (str "Exception: " (.getMessage e)))
                (timbre/error "---------------------------------------------------")
                (spit "clock-error.log" (str (utils/now-str) " radar-timer-fn Exception: " (.getMessage e) "\n") :append true)
                (timbre/error (Exception. e))))))

(defn set-screen
    [the-frame args]
    (let [num-args (count args)
          arg-num  (if (and (> num-args 0) (utils/is-pos-int-str? (first args))) (utils/parse-int (first args)) 0)
          screens  (utils/get-screens)
          screen-num (if (>= arg-num (count screens)) 0 arg-num)]
        (sc/move! the-frame :to [(:x (nth screens screen-num)) (:y (nth screens screen-num))])))

(defn setup-config
    []
    (conf/read-config-file)
    (timbre/info "starting watch")
    (let [changes (aw/changes-in ["./"])]
        (async/go (while true
            (let [[op filename] (<! changes)]
                ;; op will be one of :create, :modify or :delete
                (if (and (= op :modify) (= filename "./smhi-config.xml"))
                    (do
                        (timbre/info "smhi-config.xml updated")
                        (conf/read-config-file)
                        (sc/repaint! smhi-frame)))))))
    (timbre/info "after watch"))

(defn -main
    [& args]
    (timbre/merge-config! {:appenders {:println {:enabled? false}}})
    (timbre/merge-config!
        {:appenders
        	{:spit
        		(utils/max-size-appender
        			{:path "./" :prefix "clock" :max-size 1000000 :num-files 10
                     :output-fn (partial timbre/default-output-fn {:stacktrace-fonts {}})})}})
    
    (setup-config)
    (utils/setup-images)

    (gutils/set-background)
    (-> smhi-frame (set-screen args) sc/show!)

    ; set timer for clock 1s
    (st/timer (fn [x] (sc/repaint! (sc/select smhi-frame [:#clock]))))

    ; set timer for weather forecast 30 minutes
    (st/timer weather-timer-fn
        :initial-delay (* 1000 2)
        :delay (* 1000 60 30))

    ; set timer for radar image paint 100ms
    (st/timer  (fn [x] (sc/repaint! (sc/select smhi-frame [:#radar])))
        :initial-delay (* 1000 5)
        :delay (/ 1000 (:radar-fps @conf/config)))

    ; set timer for radar image download 5 minutes
    (st/timer radar-timer-fn
        :delay (conf/radar-interval-ms))
)
