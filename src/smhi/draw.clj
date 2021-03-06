(ns smhi.draw
    (:require 	(smhi 			[utils         :as utils]
              					[graph-utils   :refer :all]
              					[images        :refer :all]
              					[config        :refer [config]])
            	(clj-time 		[core          :as t]
              					[format        :as f]
              					[local         :as l])
              	(seesaw 		[timer         :as st]
              					[core          :as sc]
              					[border        :as sb]
              					[graphics      :as sg]
              					[color         :as sclr]
              					[font          :as sf]
              					[dev           :as sd])
              	(taoensso 		[timbre        :as log]))
    (:import (javax.swing 	JFrame JLabel)
             (java.awt 		Color Font FontMetrics GraphicsEnvironment)
             (java.awt.font TextLayout)
             (java.io 		ByteArrayInputStream)
             (javax.imageio ImageIO)
             (java.io 		File)))

;;-----------------------------------------------------------------------------

(defn draw-clock
  	"draw the clock"
	[widget ^java.awt.Graphics2D g2d]
	;(log/trace "draw-clock")
    (try
		(let [width        (.getWidth widget)
        	  height       (.getHeight widget)
        	  now          (l/local-now)
	    	  sec          (t/second now)
	    	  minute       (+ (t/minute now) (/ sec 60.0))
	    	  hour         (+ (t/hour now) (/ minute 60.0))
	    	  rotated-hour (center-rotate (clock-pics :hour-hand) (utils/hour-to-angle hour))
	    	  rotated-min  (center-rotate (clock-pics :min-hand) (* minute 6))
	    	  rotated-sec  (center-rotate (clock-pics :sec-hand) (* sec 6))]
	    	(draw-image g2d width height rotated-hour :min :center :center "hour")
	    	(draw-image g2d width height rotated-min  :min :center :center "minute")
	    	(draw-image g2d width height rotated-sec  :min :center :center "second"))
	    (catch Exception e
	    	(log/error e))))

;;-----------------------------------------------------------------------------

(defn draw-text
  	"draw text with a circle background"
  	[^java.awt.Graphics2D g2d x y txt txt-style left-side]
  	(let [txt-width    (utils/string-width g2d txt-style txt)
          txt-height   (utils/string-height g2d txt-style)
          radius       (+ (/ txt-height 2) 4)
          circle-x     (if left-side (- x (/ radius 2) 5) (+ x (/ radius 2) 5))
          txt-y        (+ y (/ txt-height 4))
          txt-x        (- circle-x (/ txt-width 2))]
	    (sg/draw g2d
	        (sg/circle circle-x y radius)
	        (config :text-circle-style))
	    (sg/draw g2d
	        (sg/string-shape txt-x txt-y txt)
	        txt-style)))

;;-----------------------------------------------------------------------------

(defn draw-background
  	"draw the overall background"
    [widget ^java.awt.Graphics2D g2d]
  	;(log/trace "draw-background")
    (try
        (when (some? @landscape-pic)
            (draw-image g2d (.getWidth widget) (.getHeight widget) @landscape-pic :both :center :center "background"))
        (catch Exception e
            (log/error e))))

(defn draw-exception-txt
    [^java.awt.Graphics2D g2d width height e]
    (let [txt      (.getMessage e)
          t-width  (utils/string-width  g2d (config :exception-style) txt)
          t-height (utils/string-height g2d (config :exception-style))]
        (sg/draw g2d
            (sg/string-shape (/ (- width t-width) 2)
                          (/ (- height t-height) 2)
                          txt)
            (config :exception-style))))
