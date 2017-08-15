(ns smhi.forecast
    (:require 	(smhi 			[utils         :refer :all]
              					[draw          :refer :all]
              					[weather       :refer :all]
              					[images        :refer :all]
              					[graph-utils   :refer :all]
              					[sun           :refer :all]
              					[date          :refer :all]
              					[config        :refer :all])
              	(clj-time 		[core          :as t]
              					[format        :as f]
              					[local         :as l])
            	(clojure.math 	[numeric-tower :as math])
             	(clojure.spec 	[alpha         :as s])
            	(clojure  		[string        :as str])
              	(seesaw 		[timer         :as st]
              					[core          :as sc]
              					[config        :as conf]
              					[selector      :as sel]
              					[border        :as sb]
              					[graphics      :as sg]
              					[color         :as sclr]
              					[font          :as sf]
              					[dev           :as sd])
              	(taoensso 		[timbre        :as log]))
     (:import 	(javax.swing   JFrame JLabel)
             	(java.awt      Color Font FontMetrics GraphicsEnvironment)
             	(java.awt.font TextLayout)
             	(java.io       ByteArrayInputStream File)
             	(javax.imageio ImageIO)))

;;-----------------------------------------------------------------------------

(defn get-param
    "extract all the data for a specific parameter"
    [data param]
    {:pre [(q-valid? keyword? param)]
     :post [(q-valid? (s/coll-of (s/cat :x integer? :y number?)) %)]}
    (map (fn [x] (select-keys x [:minutes-x :pixels-x param])) data))

;;-----------------------------------------------------------------------------

(defn get-temp-scaling
    "calculate all the values needed for drawing the temerature graph"
    [data width height]
    (let [temp-span (- (config :max-fixed-temp) (config :min-fixed-temp))]
        {:min         (config :min-fixed-temp)                    ; lowest value
         :max         (config :max-fixed-temp)                    ; highest value
         :axis-scale  (/ temp-span (config :axis-span)) ; integer size of steps
         :axis-start  (config :min-fixed-temp)                       ; what integer does the axis start on
         :multiplier  1                           ; multiplier for data
         :y-scale     (/ height temp-span)        ; scale factor for window
         :temp-span   temp-span              ; lowest to highest value
         :min-padding 0}))        ; how much to pad values with

;;-----------------------------------------------------------------------------

(defn- draw-ticks
  	[g2d x max-y draw-left]
    (doseq [tick-idx (range (config :axis-span))
            :let [tw (if (even? tick-idx)
                         (config :tick-width)
                         (/ (config :tick-width) 2))]]
      	(sg/draw g2d
        	(sg/line x
            		 (- max-y (* tick-idx (/ (inc max-y) (config :axis-span))))
            		 (+ x (if draw-left (- 0 tw) tw))
            		 (- max-y (* tick-idx (/ (inc max-y) (config :axis-span)))))
        	(config :axis-style))))

(defn- draw-axis-txt
  	[g2d x max-y style f draw-left]
    (doseq [text-idx (range 2 (config :axis-span) 2)]
      	(draw-text g2d (+ x (if draw-left (neg (config :tick-width)) (config :tick-width)))
             		   (- max-y (* text-idx (/ (inc max-y) (config :axis-span))))
             		   (str (int (f text-idx)))
             		   (config style)
             		   true)))

(defn draw-left-axis
  	"draw axises for forecast graphics (and scales)"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-left-axis")
    ; left vertical line
    (sg/draw g2d
      	(sg/line (dec width) 0 (dec width) (dec height))
      	(config :axis-style))
    ; left left vertical line
    (sg/draw g2d
      	(sg/line (/ width 2) 0 (/ width 2) (dec height))
      	(config :axis-style))
    ; bottom line
    (sg/draw g2d
      	(sg/line 0 (dec height) (dec width) (dec height))
      	(config :axis-style))
    ; ticks on axises
 	(draw-ticks g2d (dec width) (dec height) true)
    (draw-ticks g2d (/ width 2) (dec height) true)
    ; axis text
 	(draw-axis-txt g2d
                   (dec width)
                   (dec height)
                   :temp-axis-text-style
                   (fn [i] (+ (config :min-fixed-temp) (* i (/ (- (config :max-fixed-temp) (config :min-fixed-temp)) (config :axis-span)))))
                   true)
    (draw-axis-txt g2d
                   (/ width 2)
                   (dec height)
                   :rain-axis-text-style
                   (fn [i] (/ i (/ (config :axis-span) (rain-axis-span))))
                   true))

(defn draw-right-axis
  	"draw axises for forecast graphics (and scales)"
  	[^java.awt.Graphics2D g2d width height]
  	(log/trace "draw-right-axis")
	; right vertical line
    (sg/draw g2d
      	(sg/line 0 0 0 (dec height))
      	(config :axis-style))
    ; bottom line
    (sg/draw g2d
      	(sg/line 0 (dec height) (dec width) (dec height))
      	(config :axis-style))
    ; ticks on axises
    (draw-ticks g2d 0 (dec height) false)
    ; axis text
    (draw-axis-txt g2d
               	   (config :right-axis-extra)
                   (dec height)
                   :wind-axis-text-style
                   (fn [i] (* i (config :wind-axis-factor)))
                   false))

;;-----------------------------------------------------------------------------

(defn- widget-offset
  	[widget]
    {:post [(q-valid? (s/int-in 0 7) %)]}
   	(-> widget (sel/id-of) name (str/replace "day-" "") Integer/valueOf))

;;-----------------------------------------------------------------------------

(defn draw-weekday
	[^java.awt.Graphics2D g2d width height day-offset]
  	(let [day-date     (t/plus (l/local-now) (t/days day-offset))
		  day-idx      (mod (+ (day-of-week) day-offset) 7)
		  day-str      (nth ["M" "T" "O" "T" "F" "L" "S"] day-idx)
		  stroke-style (sg/style :foreground :white :background :white)
          fill-style   (sg/style :foreground :black :background :black :font (config :day-font))
          red-style    (sg/style :foreground :red :background :red :font (config :day-font))
    	  outline      (.getOutline (TextLayout. day-str (config :day-font) (.getFontRenderContext g2d)) nil)
		  stroke-out   (.createStrokedShape (sg/stroke :width (config :day-stroke-width)) outline)
		  txt-x        (- (/ width 2) (/ (string-width g2d fill-style day-str) 2))
		  txt-y        (+ (/ height 2) (/ (.height (.getBounds outline)) 2))
		  day-fill     (if (red-day? day-date) red-style fill-style)]
        (sg/draw g2d
      		(sg/string-shape txt-x txt-y day-str)
      		day-fill)
        (sg/push g2d
        	(-> g2d
            	(sg/translate txt-x txt-y)
            	(sg/draw stroke-out stroke-style)))))

(defn draw-day
  	[^java.awt.Graphics2D g2d width height day-offset]
   	;(log/trace "draw-day" (.getX widget) (.getY widget) (.getWidth widget) (widget-offset widget))
  	(try
      	(fill               g2d width height (config :forecast-bg))
        (draw-sunrise  g2d width height)
        (draw-line-seq g2d [[0 0] [(dec width) 0]
                            [(dec width) (dec height)]
                            [0 (dec height)]] (sg/style :foreground :white :stroke 1))
		(draw-weekday       g2d width height day-offset)
		(catch Exception e
	      	(log/error e "draw-day"))))

;;-----------------------------------------------------------------------------

(defn find-closest
    ""
    [width data]
;    {:pre [(q-valid? (s/coll-of (s/cat :x integer? :y number?)) data)]
;     :post [(q-valid? (s/coll-of :symb/symb-target) %)]}
    (let [num-dists (* (config :graph-days) (inc (config :symbols-per-day)))
          day-width (/ width (config :graph-days))
          symbol-dist (/ width num-dists)]
      	(map (fn [target-px] (as-> data $
                                   (map (fn [m] (assoc m :dist (abs (- target-px (:pixels-x m)))
                                                         :target target-px)) $)
                                   (sort-by :dist $)
                                   (first $)
                                   (assoc (select-keys $ [:target :minutes-x :pixels-x]) :value (-> $ :values :Wsymb))))
            (flatten
              	(for [day-idx (range (config :graph-days))]
              		(for [symbol-idx (range 1 (inc (config :symbols-per-day)))]
                 		(int (+ (* day-idx day-width) (* symbol-idx symbol-dist)))))))))

(defn draw-graph-symbols
  	"draw the weather symbols on the forecast"
  	[^java.awt.Graphics2D g2d width height]
  	(when (some? @weather-data)
    	(doseq [st (find-closest width @weather-data)]
       		(sg/draw g2d
                  	 (sg/image-shape (- (:target st) (/ (.getWidth (get-tiny-symbol 0)) 2))
                                     0
                                     (get-tiny-symbol (:value st)))
                     nil))))

;;-----------------------------------------------------------------------------

(defn draw-clouds
  	"draw graphic forecast of cloud cover"
  	[^java.awt.Graphics2D g2d width height]
  	(let [points  (map (fn [x] [(:pixels-x x)
                             	(->> x :values :tcc_mean (* (/ height 8)) (- (dec height)))])
        			  @weather-data)]
    	(draw-line-seq g2d points (config :cloud-style))))

;;-----------------------------------------------------------------------------

(defn draw-rain
  	"draw graphic forecast of rain"
  	[^java.awt.Graphics2D g2d width height]
  	(let [y-scale    (/ height (config :max-rain-level))
          points     (map (fn [x] [(:pixels-x x)
                                   (->> x :values :pmedian (* y-scale) (- (dec height)))])
                          @weather-data)
          all-points (concat [[(-> points first first) (dec height)]]
                      		 points
                      		 [[(-> points last first) (dec height)]])]
     	(sg/draw g2d
	             (apply sg/polygon all-points)
	             (config :rain-style))
	    (draw-line-seq g2d points (sg/style :foreground :white))))

;;-----------------------------------------------------------------------------

(defn draw-temp
  	"draw graphic forecast of temperature"
  	[^java.awt.Graphics2D g2d width height]
  	(let [temp-data (map (fn [x] [(:pixels-x x) (-> x :values :t)]) @weather-data)
	      temp-info (get-temp-scaling temp-data width height)
	      points (map (fn [x] [(first x)
                      		   (- (dec height)
                              	  (-> x
                                      second
                                 	  (- (:min         temp-info))
                                 	  (* (:multiplier  temp-info))
                                 	  (+ (:min-padding temp-info))
                                 	  (* (:y-scale     temp-info))
                                      int))])
                	temp-data)]
     	(draw-line-seq g2d points (config :temp-style))))

;;-----------------------------------------------------------------------------

(defn draw-wind
  	"draw graphic forecast of wind"
  	[^java.awt.Graphics2D g2d width height]
  	(let [max-gust  (+ (apply max (map #(-> % :values :gust) @weather-data)) (config :wind-padding))
          y-scale   (/ height (* (config :axis-span) (config :wind-axis-factor)))
          g-points  (map (fn [x] [(:pixels-x x)
                                  (->> x :values :gust (* y-scale) (- (dec height)))])
                         @weather-data)
          s-points  (map (fn [x] [(:pixels-x x)
                                  (->> x :values :ws (* y-scale) (- (dec height)))])
                         (reverse @weather-data))]
    	(sg/draw g2d
        	     (apply sg/polygon (concat g-points s-points))
          		 (config :wind-style))))

;;-----------------------------------------------------------------------------

(defn draw-forecast
  	"draw the forecast graphics"
  	[^java.awt.Graphics2D g2d width height]
    (try
		(draw-graph-symbols g2d width height)
		(draw-wind    		g2d width height)
        (draw-clouds  		g2d width height)
        (draw-rain    		g2d width height)
        (draw-temp    		g2d width height)
     	(when (some? @weather-exception)
	        (draw-exception-txt g2d width height @weather-exception))
	    (catch Exception e
	      	(log/error e "draw-curve"))))

;;-----------------------------------------------------------------------------

;ids (vec (concat [:#left-axis :#right-axis
;                                      :#wnow-temp :#wnow-humidity :#wnow-cloud :#wnow-wind
;			        			  	  :#wnow-symbol :#wnow-barometric :#wnow-rain
;			        			  	  :#wnow-thunder :#wnow-direction-txt :#wnow-direction-symb]
;                                 (for [day-idx (range (config :graph-days))]
;                                       (->> day-idx (str "#day-") keyword))))
