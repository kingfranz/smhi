(ns smhi.graph-utils
    (:require 	(smhi 			[utils         :refer :all]
            					[config        :refer [config]])
              	(clj-time 		[core          :as t]
            					[format        :as f]
            					[local         :as l])
              	(clojure.java 	[io            :as io])
            	(clojure.math 	[numeric-tower :as math])
            	(seesaw 		[timer         :as st]
            					[core          :as sc]
            					[border        :as sb]
            					[graphics      :as sg]
            					[color         :as sclr]
            					[font          :as sf])
            	(image-resizer  [resize        :refer :all]
                                [core          :refer :all]
                                [crop          :refer :all]
                                [scale-methods :refer :all])
              	(taoensso 		[timbre        :as log])))

;;-----------------------------------------------------------------------------

(defn image-exists?
    [fname]
    (log/info "image-exists?:" fname)
    (.exists (io/as-file (str (when-not (clojure.string/includes? fname "/") (config :image-dir)) fname))))

(defn read-image
    [fname]
    (log/info "read-image:" fname)
    (javax.imageio.ImageIO/read (java.io.File.
        (str (when-not (clojure.string/includes? fname "/") (config :image-dir)) fname))))

(defn write-image
    [fname image]
    (log/info "write-image:" fname)
    (javax.imageio.ImageIO/write image "png" (java.io.File.
		(str (when-not (clojure.string/includes? fname "/") (config :image-dir)) fname)))
    image)

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

(defn scale-image
    "scale and crop an image"
    [target-width target-height image]
    (let [src-width     (.getWidth image)
          src-height    (.getHeight image)
          src-ratio     (/ src-width src-height)
          target-ratio  (/ target-width target-height)
          eq            (fn [v1 v2] (= (int (* v1 100)) (int (* v2 100))))
          crop-func     (cond
                          	(eq src-ratio target-ratio)
                           		(fn [x] x)
                           	(> src-ratio target-ratio)
                            	(crop-fn (/ (- src-width (* src-height target-ratio)) 2)
                                      	 0
                                         (* src-height target-ratio)
                                         src-height)
                            :else
                            	(crop-fn 0
                                         (/ (- src-height (/ src-width target-ratio)) 2)
                                         src-width
                                      	 (/ src-width target-ratio)))
          new-img       (->> image
              				 (crop-func)
              				 ((resize-fn target-width target-height ultra-quality)))
          ]
        ;(write-image (str "img-" (rand-int 100) ".png") new-img)
;      	(println "TW" (int target-width) "TH" (int target-height)
;                 "SW" (int src-width) "SH" (int src-height)
;                 "SR" (float src-ratio) "TR" (float target-ratio)
;                 (.getWidth new-img) (.getHeight new-img))
        new-img))

;;-----------------------------------------------------------------------------

(defn draw-image
    "draw a image within a widget"
    [^java.awt.Graphics2D g2d image]
    ;(println "draw-image" (.getWidth image) (.getHeight image))
    (as-> image $
          (sg/image-shape 0 0 $)
          (sg/draw g2d $ nil)))

