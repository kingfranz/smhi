(ns smhi.radar
    (:require 	(smhi 			[utils         :as utils]
              					[graph-utils   :as gutils]
                   				[draw          :refer :all]
                   				[images        :refer [clock-pics set-background]]
              					[config        :refer :all])
              	(clojure.spec 	[alpha         :as s])
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
              	(taoensso 		[timbre        :as log])))

;;-----------------------------------------------------------------------------

; queue of radar images
(def radar-queue (atom nil))

; the current radar queue
(def radar-data (atom nil))

; not nil if we got an exception
(def radar-exception (atom nil))

;;-----------------------------------------------------------------------------

(defn mk-radar-image
    [radar-pic]
    (let [map-pic      (clock-pics :map-pic)
          map-width    (.getWidth map-pic)
          map-height   (.getHeight map-pic)
          sub-radar    (.getSubimage radar-pic
                                     (config :radar-sub-upper-left-x)
                                     (config :radar-sub-upper-left-y)
                                     (config :radar-sub-width)
                                     (config :radar-sub-height))
          width-ratio  (/ (- map-width  (* (config :radar-border-size) 2)) (config :radar-sub-width))
          height-ratio (/ (- map-height (* (config :radar-border-size) 2)) (config :radar-sub-height))
          buffer       (sg/buffered-image map-width map-height)
          buffer-g2d   (.createGraphics buffer)
          time-txt     (utils/hour-minute)
          time-width   (utils/string-width buffer-g2d (config :radar-txt-style) time-txt)
          time-height  (utils/string-height buffer-g2d (config :radar-txt-style))]
        (sg/draw buffer-g2d
          	(sg/image-shape 0 0 map-pic)
            nil)
        (sg/push buffer-g2d
          	(sg/scale buffer-g2d width-ratio height-ratio)
        	(sg/draw buffer-g2d
          		(sg/image-shape 1 1 sub-radar)
            	nil))
        (sg/draw buffer-g2d
            (sg/string-shape (- (config :radar-txt-x) time-width)
                             (- (config :radar-txt-y) time-height)
                             time-txt)
            (config :radar-txt-style))
        buffer))

(defn have-radar-data
    []
    (if (not-empty @radar-data)
        true
        (do
            (when-let [queue @radar-queue]
                (reset! radar-data (apply conj queue (repeat (* (config :radar-fps)
                                                                (config :radar-ani-delay-sec))
                                                             (last queue)))))
            (not-empty @radar-data))))

(defn get-current-radar-image
    []
    (when (have-radar-data)
        (let [image (peek @radar-data)]
            (reset! radar-data (pop @radar-data))
            image)))

(defn draw-radar
  	"draw the radar picture"
	[widget ^java.awt.Graphics2D g2d]
	;(log/trace "draw-radar")
    (try
		(when-let [image (get-current-radar-image)]
			(sg/draw g2d (sg/image-shape 0 0 image) nil))
		(if (some? @radar-exception)
			(draw-exception-txt g2d (.getWidth widget) (.getHeight widget) @radar-exception))
		(catch Exception e
			(log/error e))))

;;-----------------------------------------------------------------------------

(defn add-radar-2-queue
    [image]
    (when (nil? @radar-queue)
        (reset! radar-queue (clojure.lang.PersistentQueue/EMPTY)))
    (when (> (count @radar-queue) (max-radar-queue-size))
        (reset! radar-queue (pop @radar-queue)))
    (reset! radar-queue (conj @radar-queue image)))

(defn get-radar-image
    "get latest radar image from SMHI"
    []
    (let [data    (utils/send-request (config :radar-url) :image)
          img     (utils/byte-array-2-image data)
          imgtype (java.awt.image.BufferedImage/TYPE_INT_ARGB)
          width   (.getWidth img)
          height  (.getHeight img)
          simg    (java.awt.image.BufferedImage. width height imgtype)
          g2d     (.createGraphics simg)]
     (.drawImage g2d img 0 0 width height nil)
     (.dispose g2d)
     simg))

(defn radar-timer-fn
    []
    (try
	    (log/info "getting new radar image")
	    (-> (get-radar-image)
	        (mk-radar-image)
	        (add-radar-2-queue))
	    (reset! radar-exception nil)
        (catch Exception e
            (reset! radar-exception e)
            (log/error (str "\n---------------------------------------------------\n"
            				"Exception in radar-timer-fn: " (.getMessage e) "\n"
            				"---------------------------------------------------\n"
            				e "\n")))))

;;-----------------------------------------------------------------------------

