(ns smhi.clock
    (:require 	(smhi 			[utils         :refer :all]
              					[graph-utils   :refer :all]
              					[images        :refer :all]
              					[sun           :refer :all]
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
              	(taoensso 		[timbre        :as log])))

;;-----------------------------------------------------------------------------

(defn draw-analog
  	"draw the clock"
	[widget ^java.awt.Graphics2D g2d]
	;(log/trace "draw-clock")
    (try
		(let [now    (l/local-now)
	    	  sec    (t/second now)
	    	  minute (+ (t/minute now) (/ sec 60.0))
	    	  hour   (+ (* (mod (t/hour now) 12) 5) (* (/ minute 60.0) 5))]
    		(draw-image g2d (get-pic :clock-pic))
      		(write-sun-info g2d (.getWidth widget) (.getHeight widget))
	    	(draw-image g2d (get-hourhand (int hour)))
	    	(draw-image g2d (get-minutehand (int minute)))
	    	(draw-image g2d (get-secondhand (int sec))))
	    (catch Exception e
	    	(log/error e))))

;;-----------------------------------------------------------------------------

(defn draw-bar
  	[g2d ulx uly width height style value]
   	(let [stroke-width (.getLineWidth (:stroke style))
          bkrd-width  (- width (* stroke-width 2))
          bkrd-height (- height (* stroke-width 2))
          value-width (* bkrd-width value)]
      	(sg/push g2d
	        (sg/translate g2d ulx uly)
	    	(fill g2d width height (sclr/color :black))
	        (sg/translate g2d stroke-width stroke-width)
	        (fill g2d bkrd-width bkrd-height (:background style))
	        (fill g2d value-width bkrd-height (:foreground style))
        )))

(defn draw-digital
  	[widget ^java.awt.Graphics2D g2d]
    (let [width      (.getWidth widget)
          height     (.getHeight widget)
          d-str      (date-str)
          d-str-w    (string-width g2d (config :date-stroke-style) d-str)
          d-str-h    (string-height g2d (config :date-stroke-style) d-str)
          t-str      (time-str)
          t-str-w    (string-width g2d (config :date-fill-style) t-str)
          t-str-h    (string-height g2d (config :date-fill-style) t-str)
          dbt-str    "Today"
          dbt-str-w  (string-width g2d (config :bar-fill-style) dbt-str)
          dbt-str-h  (string-height g2d (config :bar-fill-style) dbt-str)
          mbt-str    "Month"
          mbt-str-w  (string-width g2d (config :bar-fill-style) mbt-str)
          mbt-str-h  (string-height g2d (config :bar-fill-style) mbt-str)
          ybt-str    "Year"
          ybt-str-w  (string-width g2d (config :bar-fill-style) ybt-str)
          ybt-str-h  (string-height g2d (config :bar-fill-style) ybt-str)
          bar-width  (* width (config :bar-width-percent))
          bar-height (* height (config :bar-height-percent))
          bar-lx     (half (- width bar-width))
          day-frac   (/ (t/in-seconds (t/interval (t/with-time-at-start-of-day (l/local-now)) (l/local-now)))
                        (* 24 60 60))
          month-frac (/ (+ (t/day (l/local-now)) day-frac) (t/number-of-days-in-the-month (l/local-now)))
          year-frac  (/ (+ (t/month (l/local-now)) month-frac) 12)
          calc-y     (fn [tag h] (- (* height (config tag)) h))
          ]
      	; draw date
        (draw-outlined-char g2d
                            (config :date-fill-style)
                            (config :date-stroke-style)
                            (half (- width d-str-w))
                            (calc-y :date-str-y d-str-h)
                            d-str)
        
	    ; draw time
        (draw-outlined-char g2d
                            (config :date-fill-style)
                            (config :date-stroke-style)
                            (half (- width t-str-w))
                            (calc-y :time-str-y t-str-h)
                            t-str)
        
	    ; draw day bar title
        (draw-outlined-char g2d
                            (config :bar-fill-style)
                            (config :bar-stroke-style)
                            (half (- width dbt-str-w))
                            (calc-y :today-bar-y 10)
                            dbt-str)
	    ; draw day bar
     	(draw-bar g2d
                  bar-lx
                  (calc-y :today-bar-y 0)
                  bar-width
                  bar-height
                  (config :bar-style)
                  day-frac)
      
	    ; draw month bar title
        (draw-outlined-char g2d
                            (config :bar-fill-style)
                            (config :bar-stroke-style)
                            (half (- width mbt-str-w))
                            (calc-y :month-bar-y 10)
                            mbt-str)
	    ; draw month bar
     	(draw-bar g2d
                  bar-lx
                  (calc-y :month-bar-y 0)
                  bar-width
                  bar-height
                  (config :bar-style)
                  month-frac)
      
	    ; draw year bar title
        (draw-outlined-char g2d
                            (config :bar-fill-style)
                            (config :bar-stroke-style)
                            (half (- width ybt-str-w))
                            (calc-y :year-bar-y 10)
                            ybt-str)
	    ; draw year bar
     	(draw-bar g2d
                  bar-lx
                  (calc-y :year-bar-y 0)
                  bar-width
                  bar-height
                  (config :bar-style)
                  year-frac)
      ))

(defn draw-segment
  	[^java.awt.Graphics2D g2d cx cy r-inner r-outer s-angle e-angle fg bg]
    (let [o-ulx  (- cx r-outer)
          o-uly  (- cy r-outer)
          o-side (* r-outer 2)
          i-ulx  (- cx r-inner)
          i-uly  (- cy r-inner)
          i-side (* r-inner 2)
          o-pie  (sg/pie o-ulx o-uly o-side o-side s-angle e-angle)
          i-pie  (sg/pie i-ulx i-uly i-side i-side s-angle e-angle)
          o-area (java.awt.geom.Area. o-pie)
          i-area (java.awt.geom.Area. i-pie)]
      	;(sg/draw g2d
        ; 	(sg/circle cx cy r-outer)
        ;  	(sg/style :foreground bg))
        (.subtract o-area i-area)
        (.setPaint g2d (sclr/color :black))
        (.setStroke g2d (sg/stroke :width 2))
      	(.draw g2d o-area)
        (.setPaint g2d fg)
      	(.fill g2d o-area)))

(defn draw-sttng*
  	[widget ^java.awt.Graphics2D g2d out-r w frac fg]
    (let [width      (.getWidth widget)
          height     (.getHeight widget)
          ]
      	(draw-segment g2d
                      (/ width 2)
                      (/ height 2)
                      (* (/ width 2) (- out-r w))
                      (* (/ width 2) out-r)
                      90
                      (neg (* frac 360))
                      fg
                      (sclr/color 0 0 0 0))))

(defn draw-sttng
  	[widget ^java.awt.Graphics2D g2d]
    (let [second-frac (/ (t/second (l/local-now)) 60)
          minute-frac (/ (t/minute (l/local-now)) 60)
          hour-frac   (/ (t/hour (l/local-now))   24)
          day-frac    (/ (t/in-seconds (t/interval (t/with-time-at-start-of-day (l/local-now)) (l/local-now)))
                         (* 24 60 60))
          month-frac  (/ (t/day (l/local-now))    (t/number-of-days-in-the-month (l/local-now)))
          year-frac   (/ (t/month (l/local-now))  12)]
      	(draw-sttng* widget g2d 0.90 0.08 year-frac   (sclr/color   0   0 204))
       	(draw-sttng* widget g2d 0.77 0.08 month-frac  (sclr/color   1 102 255))
       	(draw-sttng* widget g2d 0.64 0.08 day-frac    (sclr/color 128 128 128))
       	(draw-sttng* widget g2d 0.51 0.08 hour-frac   (sclr/color 205 135  13))
       	(draw-sttng* widget g2d 0.38 0.08 minute-frac (sclr/color 204   0   1))
       	(draw-sttng* widget g2d 0.26 0.08 second-frac (sclr/color 254   0   1))
       	))

(defn draw-clock
  	"draw the clock"
	[widget ^java.awt.Graphics2D g2d]
	(if (< (mod (t/second (l/local-now)) (* (config :clock-switch-sec) 2)) (config :clock-switch-sec))
   		(draw-analog widget g2d)
     	(draw-sttng widget g2d)))
 