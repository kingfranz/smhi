(ns smhi.draw
    (:require 	(smhi 			[utils         :refer :all]
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

(defn draw-text-circle
  	"draw text with a circle background"
  	[^java.awt.Graphics2D g2d x y txt txt-style left-side]
  	(let [txt-width  (string-width g2d txt-style txt)
          txt-height (string-height g2d txt-style txt)
          circle-x   (if left-side
                         (- x (/ (config :axis-radius) 2))
                         (+ x (/ (config :axis-radius) 2)))
          txt-y      (+ y (/ txt-height 2))
          txt-x      (- circle-x (/ txt-width 2))]
	    (sg/draw g2d
	        (sg/circle circle-x y (config :axis-radius))
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
        (when (some? (background))
            (draw-image g2d (background)))
        (catch Exception e
            (log/error e))))

(defn draw-exception-txt
    [^java.awt.Graphics2D g2d width height e]
    (let [txt      (.getMessage e)
          t-width  (string-width  g2d (config :exception-style) txt)
          t-height (string-height g2d (config :exception-style) txt)]
        (sg/draw g2d
            (sg/string-shape (/ (- width t-width) 2)
                          (/ (- height t-height) 2)
                          txt)
            (config :exception-style))))
