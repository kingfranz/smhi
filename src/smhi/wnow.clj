(ns smhi.wnow
    (:require 	(smhi 			[utils         :refer :all]
              					[graph-utils   :refer :all]
                   				[weather       :refer [weather-data]]
                       			[images        :refer :all]
              					[config        :refer [config]]
              					[date          :as date]
              					[sun           :as sun])
              	(clojure.spec 	[alpha         :as s])
            	(clj-time 		[core          :as t]
              					[format        :as f]
              					[coerce        :as c]
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

(defn- draw-wnow-txtbox
  	"draw background, title and value for one section of now info"
  	[^java.awt.Graphics2D g2d ulx uly width height txt style]
  	(let [txt-width  (string-width  g2d style txt)
          txt-height (string-height g2d style txt)]
	    ; draw sq
	    (sg/draw g2d
	      	(sg/rounded-rect ulx
	              			 uly
	              			 width
	              			 height
	              			 (config :wnow-radius)
                   			 (config :wnow-radius))
	      	(config :info-bg-style))
	    ; draw text
	    (sg/draw g2d
	        (sg/string-shape (+ ulx (/ (- width txt-width) 2))
	                		 (+ uly (/ height 2) (/ txt-height 2))
	                		 txt)
	        style)))

(defn- draw-wnow-info
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height title value]
  	(try
     	;(draw-wnow-box g2d width height)
    	(when (some? @weather-data)
      		(let [sq-width (- width (* (config :wnow-side-border) 2))
              	  split-y  (* height (config :wnow-title-part))
                  half-center (/ (config :wnow-center-border) 2)]
          		(draw-wnow-txtbox g2d
                              	  (config :wnow-side-border)
                                  (config :wnow-top-border)
                                  sq-width
                                  (- split-y
                                     (config :wnow-top-border)
                                     half-center)
                                  title
                                  (config :wnow-title-style))
            	(draw-wnow-txtbox g2d
                               	  (config :wnow-side-border)
                                  (+ split-y half-center)
                                  sq-width
                                  (- height
                                     split-y
                                     (config :wnow-bottom-border)
                                     half-center)
                                  (str value)
                                  (config :wnow-value-style))))
    	(catch Exception e
      		(log/error e))))

(defn- v-frmt
  	[k]
   	(if (some? @weather-data)
      	(->> @weather-data
             (take 24)
             (map (fn [m] [(abs (- (c/to-long (l/local-now)) (c/to-long (:timestamp m)))) (get (:values m) k)]))
             (sort-by first)
             first
             second)
        0))

(defn draw-wnow-temp
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
    (log/trace "draw-wnow-temp")
	(let [value (if (some? @weather-data) (str (v-frmt :t) (config :degree-char)) "---")]
   		(draw-wnow-info g2d width height "Temp" value)))

(defn draw-wnow-humidity
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-wnow-humidity")
	(let [value (if (some? @weather-data) (str (v-frmt :r) "%") "---")]
		(draw-wnow-info g2d width height "Humidity" value)))

(defn draw-wnow-cloud
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-wnow-cloud")
	(let [value (if (some? @weather-data) (str (-> (v-frmt :tcc_mean) (/ 8) (* 100) int) "%") "---")]
		(draw-wnow-info g2d width height "Cloud" value)))

(defn draw-wnow-wind
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-wnow-wind")
	(let [value (if (some? @weather-data) (str (v-frmt :ws) "-" (v-frmt :gust)) "---")]
		(draw-wnow-info g2d width height "Wind Speed m/s" value)))

(defn draw-wnow-symbol
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-wnow-symbol")
	(let [direction      (if (some? @weather-data) (v-frmt :wd) 0)
          rotated-arrow  (center-rotate (get-pic :arrow-pic) (mod (+ direction 180) 360))]
 		(draw-image g2d (get-pic :compass-pic))
 		(draw-image g2d rotated-arrow)))

(defn draw-wnow-baro
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-wnow-baro")
	(let [value (if (some? @weather-data) (str (v-frmt :msl)) "---")]
		(draw-wnow-info g2d width height "Pressure" value)))

(defn draw-wnow-rain
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-wnow-rain")
	(let [value (if (some? @weather-data) (str (v-frmt :pmedian)) "---")]
		(draw-wnow-info g2d width height "Rain mm/h" value)))

(defn draw-wnow-thunder
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-wnow-thunder")
	(let [value (if (some? @weather-data) (str (v-frmt :tstm) "%") "---")]
		(draw-wnow-info g2d width height "Thunder" value)))

(defn draw-wnow-direction-txt
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-wnow-direction-txt")
	(let [value (if (some? @weather-data) (wind-dir-to-str (v-frmt :wd)) "---")]
		(draw-wnow-info g2d width height "Wind Dir" value)))

(defn draw-wnow-direction-symb
  	"draw all the parts of the now info"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-wnow-direction-symb")
	(try (let [value (if (some? @weather-data) (v-frmt :Wsymb) 1)]
		(draw-image g2d (get-symbol (int value))))
   		(catch Exception e (prn "DNDS" (some? @weather-data) (v-frmt :Wsymb)))))
    	