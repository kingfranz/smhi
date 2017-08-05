(ns smhi.graph-utils
    (:require 	(smhi 			[utils         :refer :all]
            					[config        :refer [config]]
                 				[images        :as images])
              	(clj-time 		[core          :as t]
            					[format        :as f]
            					[local         :as l])
            	(clojure.math 	[numeric-tower :as math])
            	(seesaw 		[timer         :as st]
            					[core          :as sc]
            					[border        :as sb]
            					[graphics      :as sg]
            					[color         :as sclr]
            					[font          :as sf])
            	(taoensso 		[timbre        :as log])))

;;-----------------------------------------------------------------------------

(defn fill
  	"fill a graphics context with color"
  	[^java.awt.Graphics2D g2d width height color]
  	(do
      	(.setPaint g2d color)
      	(.fillRect g2d 0 0 width height)
      	g2d))

;;-----------------------------------------------------------------------------

(defn draw-line-seq
  	"draw a list of lines"
  	[^java.awt.Graphics2D g2d points draw-style]
  	(doseq [pair (partition 2 1 points)]
    	(sg/draw g2d
          	(sg/line (-> pair first first)
              		 (-> pair first second)
              		 (-> pair second first)
              		 (-> pair second second))
          	draw-style)))

;;-----------------------------------------------------------------------------

(defn center-rotate
  	"rotate an image around its center"
    [image angle]
    (let [width  (.getWidth image)
          height (.getHeight image)
          buffer (sg/buffered-image width height)]
        (-> buffer
          	.createGraphics
          	(sg/translate (/ width 2) (/ height 2))
          	(sg/rotate angle)
          	(sg/translate (neg (/ width 2)) (neg (/ height 2)))
          	(sg/draw (sg/image-shape 0 0 image) nil))
        buffer))

;;-----------------------------------------------------------------------------

(defn draw-image
    "draw a (possibly scaled) image within a widget"
    [^java.awt.Graphics2D g2d width height image match valign halign iname]
    {:pre [(some #{match}  '(:both :width :height :min :max))
           (some #{valign} '(:top :center :bottom))
           (some #{halign} '(:left :center :right))]}
    (let [image-width     (.getWidth image)
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

