(ns smhi.frame
    (:require 	(smhi 			[wnow     :refer :all]
                     			[forecast :refer :all]
              					[radar    :refer :all]
              					[draw     :refer :all]
              					[config   :refer [config]])
              	(clj-time 		[core     :as t]
              					[format   :as f]
              					[local    :as l])
              	(seesaw 		[core     :as sc]
              					[color    :as sclr]
              					[font     :as sf])
              	(taoensso 		[timbre   :as log])))

;;-----------------------------------------------------------------------------

(def ^:private smhi-frame-store (atom nil))

(defn- wnow-coord
    [x y]
    [(* (config :wnow-width) x)
     (* (config :wnow-height) y)
     (config :wnow-width)
     (config :wnow-height)])

(defn- mk-smhi-frame
    "create the frame"
    []
    (sc/window
        :width (config :horiz-res)
        :height (config :vert-rez)
        :content
        (sc/xyz-panel :items (concat [
            ;----------------------------------------------
            ; weather now area
            ;----------------------------------------------
            (sc/label  :id         :wnow-temp
                       :bounds     (wnow-coord 0 0)
                       :background (config :widget-style)
                       :foreground (config :widget-style)
                       :paint      nil)
            
            (sc/label  :id         :wnow-humidity
                       :bounds     (wnow-coord 1 0)
                       :background (config :widget-style)
                       :foreground (config :widget-style)
                       :paint      nil)
            
            (sc/label  :id         :wnow-cloud
                       :bounds     (wnow-coord 2 0)
                       :background (config :widget-style)
                       :foreground (config :widget-style)
                       :paint      nil)
            
            (sc/label  :id         :wnow-wind
                       :bounds     (wnow-coord 3 0)
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)
            
            (sc/label  :id         :wnow-symbol
                       :bounds     (wnow-coord 4 0)
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)
            
            (sc/label  :id         :wnow-baro
                       :bounds     (wnow-coord 0 1)
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)
            
            (sc/label  :id         :wnow-rain
                       :bounds     (wnow-coord 1 1)
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)
            
            (sc/label  :id         :wnow-thunder
                       :bounds     (wnow-coord 2 1)
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)
            
            (sc/label  :id         :wnow-direction-txt
                       :bounds     (wnow-coord 3 1)
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)
            
            (sc/label  :id         :wnow-direction-symb
                       :bounds     (wnow-coord 4 1)
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)

            ;----------------------------------------------
            ; radar
            ;----------------------------------------------
            (sc/label  :id         :radar
                       :bounds     [0
                                    (config :info-height)
                                    (config :radar-width)
                                    (config :radar-height)]
                       ;:background (config :widget-style)
                       :paint      draw-radar)
            
            ;----------------------------------------------
            ; clock
            ;----------------------------------------------
            (sc/label  :id         :clock
                       :bounds     [(config :radar-width)
                                    0
                                    (config :clock-width)
                                    (config :clock-height)]
                       :listen     [:mouse-clicked (fn [e] (java.lang.System/exit 0))]
                       ;:background (config :widget-style)
                       :paint      draw-clock)
            
            ;----------------------------------------------
            ; forecast area
            ;----------------------------------------------
            (sc/label  :id         :forecast
                       :bounds     [(config :left-axis-width)
                                    (config :clock-height)
                                    (- (config :graphics-width)
                                       (config :left-axis-width)
                                       (config :right-axis-width))
                                    (config :graphics-height)]
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)
            
            ;----------------------------------------------
            ; forecast area - left axis
            ;----------------------------------------------
            (sc/label  :id         :left-axis
                       :bounds     [0
                                    (config :clock-height)
                                    (config :left-axis-width)
                                    (config :graphics-height)]
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)
            
            ;----------------------------------------------
            ; forecast area - right axis
            ;----------------------------------------------
            (sc/label  :id         :right-axis
                       :bounds     [(- (config :graphics-width) (config :right-axis-width))
                                    (config :clock-height)
                                    (config :right-axis-width)
                                    (config :graphics-height)]
                       :foreground (config :widget-style)
                       :background (config :widget-style)
                       :paint      nil)
            
            ;----------------------------------------------
            ; background
            ;----------------------------------------------
            (sc/label  :id         :lbl-back
                       :bounds     [0
                                    0
                                    (config :horiz-res)
                                    (config :vert-rez)]
                       :paint      draw-background)
        	]
            ;----------------------------------------------
            ; forecast area - days
            ;----------------------------------------------
            (for [day-idx (range (config :graph-days))
                  :let [days-width (- (config :graphics-width)
                                      (config :left-axis-width)
                                      (config :right-axis-width))
                        day-width (/ days-width (config :graph-days))
                        day-x (+ (config :left-axis-width) (* day-width day-idx))]]
                (sc/label :id     (->> day-idx (str "day-") keyword)
                          :bounds [day-x
                                   (config :clock-height)
                                   day-width
                                   (config :graphics-height)]
                          :foreground (config :widget-style)
                          :background (config :widget-style)
                          :paint      nil))))))

(defn setup-frame
  	[]
   	(reset! smhi-frame-store (mk-smhi-frame)))

(defn smhi-frame
  	[]
   	(when (nil? @smhi-frame-store)
      	(setup-frame))
    @smhi-frame-store)
