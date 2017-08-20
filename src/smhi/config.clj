(ns smhi.config
	(:require 	(clojure.tools.reader 	[edn      :as edn])
				(seesaw 				[core     :as sc]
				 						[border   :as sb]
				 						[graphics :as sg]
				 						[color    :as sclr]
				 						[font     :as sf])
            	(clojure.math 			[numeric-tower :as math])
                (taoensso               [timbre      :as log])
    			(clojure				[pprint   :as pp]
                    					[string   :as str])))

;;-----------------------------------------------------------------------------

(def config-name "smhi-config.edn")

(def units
  	{:msl      {:str "Air Pressure"            :is-int true  :keep true  :unit "hPa"}
	 :t        {:str "Temperature"             :is-int true  :keep true  :unit "C"}
	 :vis      {:str "Visibility"              :is-int false :keep false :unit "km"}
	 :wd       {:str "Wind Dir"                :is-int true  :keep true  :unit "degree"}
	 :ws       {:str "Wind Speed"              :is-int true  :keep true  :unit "m/s"}
	 :r        {:str "Humidity"                :is-int true  :keep true  :unit "%"}
	 :tstm     {:str "Thunder"                 :is-int true  :keep true  :unit "%"}
	 :tcc_mean {:str "Total Clouds"            :is-int true  :keep true  :unit "octas"}
	 :lcc_mean {:str "Low Clouds"              :is-int true  :keep false :unit "octas"}
	 :mcc_mean {:str "Medium Clouds"           :is-int true  :keep false :unit "octas"}
	 :hcc_mean {:str "High Clouds"             :is-int true  :keep false :unit "octas"}
	 :gust     {:str "Gust Speed"              :is-int true  :keep true  :unit "m/s"}
	 :pmin     {:str "Min Precip"              :is-int false :keep true  :unit "mm/h"}
	 :pmax     {:str "Max Precip"              :is-int false :keep true  :unit "mm/h"}
	 :spp      {:str "Percent Frozen"          :is-int true  :keep true  :unit "%"}
	 :pcat     {:str "Precip Category"         :is-int true  :keep true  :unit "category"}
	 :pmean    {:str "Mean Precip Intensity"   :is-int true  :keep true  :unit "mm/h"}
	 :pmedian  {:str "Median Precip Intensity" :is-int true  :keep true  :unit "mm/h"}
	 :Wsymb    {:str "Symbol"                  :is-int true  :keep true  :unit "code"}})

;;-----------------------------------------------------------------------------

(def ^:private std-horizontal-res 1920)
(def ^:private std-vertical-res   1080)

(def ^:private default-config-fixed
	{
  	:horizontal-scale           1.0
    :vertical-scale             1.0
  
	:radar-sub-width            112
	:radar-sub-center-x         (+ 110 112/2)
	:radar-sub-center-y         (+ 595 45/2)
	:radar-timer-initial-sec    0
	:radar-interval-minutes     5
	:radar-fps                  10
	:radar-ani-hours            5
	:radar-ani-delay-sec        2
	:radar-url                  "http://opendata-download-radar.smhi.se/api/version/latest/area/sweden/product/comp/latest.png"
 	:radar-width-long		    3.2
   
 	:clock-fps                  1
  	:clock-delay-sec            6
   
  	:weather-timer-initial-sec  0
	:weather-timer-delay-min    30
	:wind-style                 (sg/style :foreground :white :background :lightgray :stroke 1)
	:rain-style                 (sg/style :foreground :blue :background :red :stroke 1)
	:temp-style                 (sg/style :foreground :red :background :red :stroke 8)
	:cloud-style                (sg/style :foreground :lightgreen :background :lightgreen :stroke 10)
	:axis-style                 (sg/style :foreground :white :background :white :stroke 2)
	:day-tick-style             (sg/style :foreground :white :background :white :stroke 1)
	:zero-line-style            (sg/style :foreground :white :background :white :stroke 1)
	:text-circle-style          (sg/style :foreground :white :background :white :stroke 1)
	:min-fixed-temp             -20
	:max-fixed-temp             30
	:image-dir                  "images/"
 	:image-cache-dir            "image-cache/"
 	:background-dir             "backgrounds/"
 	:master-map				    {:filename "master-map.jpg" :center-x 2187 :center-y 11615 :width 2100}
  
	:latitude                   58.786869
	:longitude                  14.265020
	:weather-url                "http://opendata-download-metfcst.smhi.se"
	:category                   "pmp2g"
	:version                    "2"
  
	:widget-style               (sclr/color 32 32 32 0)
	:degree-char                "\u00b0"
	:max-rain-level             3
 	:right-axis-extra           30
	:info-bg-style              (sg/style :foreground (sclr/color 32 32 32) :stroke 2
							     		 :background (sclr/color 128 128 128 128))
	:graph-days                 7
	:axis-width                 2
 	:wnow-vt-adjust             -3
 	:wnow-vv-adjust             -19
  	:forecast-bg		        (sclr/color 128 128 128 128)
    :day-stroke-width           3
    :symbols-per-day            3
    :day-outline-style          (sg/style :foreground :white :stroke 3 :font "ARIAL-192")
    :day-style                  (sg/style :foreground :black :stroke 3 :font "ARIAL-192")
    :day-red-style              (sg/style :foreground :red   :stroke 3 :font "ARIAL-192")
     
	:sun-box-style              (sg/style :foreground :black :stroke 2 :background (sclr/color 140 140 140))
     
	:temp-padding               5
	:wind-padding               2
	:axis-span                  10
	:wind-axis-factor           3
	:smhi-timeout               5000
	:twilight-begin             :civil_twilight_begin
	:twilight-end               :civil_twilight_end
    :wnow-title-part            1/3
    
    :bar-width-percent			0.80
    :bar-height-percent			0.10
    :date-str-y					0.20
    :time-str-y					0.30
    :today-bar-y				0.40
    :month-bar-y				0.60
    :year-bar-y					0.80
    :bar-style					(sg/style :foreground :grey :stroke 4 :background (sclr/color 140 140 140 50))
    }) 

(def ^:private default-config-var
  	{
    :radar-border-size  {:dir :both       :value 10}
    :radar-txt-x        {:dir :horizontal :value 220}
    :radar-txt-y        {:dir :vertical   :value 140}
	:sun-box-dw         {:dir :horizontal :value 50}
 	:sun-box-dh         {:dir :vertical   :value 15}
 	:sun-box-dy         {:dir :vertical   :value 40}
 	:sun-box-radius     {:dir :both       :value 50}
	:up-down-dy         {:dir :vertical   :value 100}
	:date-dy            {:dir :vertical   :value -100}
	:week-dy            {:dir :vertical   :value -180}
 	:left-axis-width    {:dir :horizontal :value 100}
	:right-axis-width   {:dir :horizontal :value 50}
	:tick-width         {:dir :horizontal :value 10}
	:wnow-top-border    {:dir :vertical   :value 9}
    :wnow-center-border {:dir :vertical   :value 6}
    :wnow-bottom-border {:dir :vertical   :value 14}
    :wnow-side-border   {:dir :horizontal :value 10}
    :wnow-radius        {:dir :both       :value 10}
    :small-symbol-sz    {:dir :both       :value 50}
    })

(def ^:private default-config-style
	{
	:radar-txt-style      {:foreground :black :font "ARIAL-BOLD" :fontsz 64}
	:temp-axis-text-style {:foreground :red :background :red :stroke 1 :font "ARIAL-BOLD" :fontsz 20}
	:wind-axis-text-style {:foreground :grey :background :grey :stroke 1 :font "ARIAL-BOLD" :fontsz 20}
	:rain-axis-text-style {:foreground :blue :background :blue :stroke 1 :font "ARIAL-BOLD" :fontsz 20}
	:wnow-title-style     {:foreground :white :font "ARIAL" :fontsz 18}
	:wnow-value-style     {:foreground :white :font "ARIAL" :fontsz 48}
	:date-txt-style       {:foreground :white :background :black :stroke 2 :font "ARIAL" :fontsz 192}
	:exception-style      {:foreground :red   :font "ARIAL" :fontsz 64}
   	:sun-txt-style        {:foreground :black :font "ARIAL" :fontsz 36}
    :date-stroke-style    {:foreground :black :font "ARIAL" :fontsz 64 :stroke 1}
    :date-fill-style      {:foreground :white :font "ARIAL" :fontsz 64 :stroke 1}
    :bar-stroke-style     {:foreground :black :font "ARIAL" :fontsz 48 :stroke 1}
  	:bar-fill-style       {:foreground :white :font "ARIAL" :fontsz 48 :stroke 1}
  	})

;;-----------------------------------------------------------------------------

(def ^:private config-store (atom nil))

;;-----------------------------------------------------------------------------

(defn horiz-res       [] (math/round (* std-horizontal-res (:horizontal-scale @config-store))))
(defn vert-res        [] (math/round (* std-vertical-res (:vertical-scale @config-store))))
(defn graphics-width  [] (math/round (horiz-res)))
(defn graphics-height [] (math/round (* (vert-res) 1/3)))
(defn clock-width     [] (math/round (* (vert-res) 2/3)))
(defn clock-height    [] (math/round (* (vert-res) 2/3)))
(defn radar-width     [] (math/round (- (horiz-res) (clock-width))))
(defn radar-height    [] (math/round (* (clock-height) 2/3)))
(defn wnow-width      [] (math/round (/ (radar-width) 5)))
(defn wnow-height     [] (math/round (/ (- (vert-res) (graphics-height) (radar-height)) 2)))

(defn scale-v
  	[hscale vscale bscale {dir :dir value :value}]
    (cond
		(= dir :horizontal) (math/round (* value hscale))
		(= dir :vertical)   (math/round (* value vscale))
		:else               (math/round (* value bscale))))

(defn do-upd
  	[s value k]
    (if (some? (get value k))
      	(sg/update-style s k (get value k))
       	s))

(defn mk-style
  	[value bscale]
    ;(println "mk-style:" (str (:font value) "-" (int (* (:fontsz value) bscale))))
	(-> (sg/style :font (str (:font value) "-" (math/round (* (:fontsz value) bscale))))
		(do-upd value :foreground)
		(do-upd value :background)
		(do-upd value :stroke)))

;;-----------------------------------------------------------------------------

(defn- convert-stroke-part
  	[s]
   	(if (nil? s)
      	nil
      	(.getLineWidth s)))

(defn- convert-rgba-part
  	[clr]
   	(if (nil? clr)
      	[]
      	[ (.getRed clr) (.getGreen clr) (.getBlue clr) (.getAlpha clr) ]))

(defn- convert-font-part
  	[fnt]
   	(if (nil? fnt)
      	""
      	(str (.getName fnt)
        	 (if (.isBold fnt) "-BOLD" "")
             (if (.isItalic fnt) "-ITALIC" "")
             "-" (.getSize fnt))))

(defrecord SMHIStyle
  	[foreground
     background
     stroke
     font])

(defn- convert-style
  	[s]
   	(SMHIStyle. (convert-rgba-part   (:foreground s))
                (convert-rgba-part   (:background s))
                (convert-stroke-part (:stroke s))
                (convert-font-part   (:font s))))

(defrecord SMHIColor
  	[color])

(defn- convert-color
  	[clr]
   	(SMHIColor. (convert-rgba-part clr)))

(defrecord SMHIFont
  	[font])

(defn- convert-font
  	[fnt]
   	(SMHIFont. (convert-font-part fnt)))

(defn- convert-config
  	[con]
   	(into {}
        (for [kv (seq con)]
           	(cond
              	(= (type (val kv)) seesaw.graphics.Style)
               		[(key kv) (convert-style (val kv))]
              	(= (type (val kv)) java.awt.Color)
               		[(key kv) (convert-color (val kv))]
              	(= (type (val kv)) java.awt.Font)
               		[(key kv) (convert-font (val kv))]
               	:else kv))))

(defn- mk-rgba
  	[v]
    (when (not (empty? v))
      	(apply sclr/color v)))

(defn- create-style
  	[value]
    (sg/style :foreground (mk-rgba (:foreground value))
              :stroke     (:stroke value)
              :background (mk-rgba (:background value))
              :font       (when (not (str/blank? (:font value))) (:font value))))

(defn- create-color
  	[value]
    (mk-rgba (:color value)))

(defn- create-font
  	[value]
    (sf/font (:font value)))

(defn write-config
  	[]
   	(log/trace "write-config ENTER")
   	(spit config-name (with-out-str (prn {
		:fixed (convert-config default-config-fixed)
  		:vars  (convert-config default-config-var)
    	:style (convert-config default-config-style)})))
    (log/trace "write-config EXIT")
   	)

(defn dump-config
  	[]
    (spit "cdump.edn" (with-out-str (pp/pprint @config-store))))

(defn load-config
  	[]
    (log/trace "load-config ENTER")
   	(write-config)
   	(let [conf (some->> config-name
             			slurp
             			(edn/read-string {:readers {'smhi.config.SMHIStyle create-style
                                         			'smhi.config.SMHIColor create-color
                                         			'smhi.config.SMHIFont  create-font}}))
          hscale (-> conf :fixed :horizontal-scale)
          vscale (-> conf :fixed :vertical-scale)
          bscale (/ (math/sqrt (+ hscale vscale)) (math/sqrt 2.0))
          vars   (into {} (map (fn [[k v]] [k (scale-v hscale vscale bscale v)]) (:vars conf)))
          styles (into {} (map (fn [[k v]] [k (mk-style v bscale)]) (:style conf)))
          ]
      	(reset! config-store (merge (:fixed conf) vars styles)))
    (log/trace "load-config EXIT")
   	)

(defn load-default-config
  	[]
   	(log/trace "load-default-config ENTER")
   	(let [vars   (into {} (map (fn [[k v]] [k (scale-v 1 1 1 v)]) default-config-var))
          styles (into {} (map (fn [[k v]] [k (mk-style v 1)]) default-config-style))]
      	(reset! config-store (merge default-config-fixed vars styles)))
    (log/trace "load-default-config EXIT"))

(def ^:private data-lock (Object.))

(defn config
  	"retrive a config value"
  	[kw]
    {:pre [(keyword? kw)]}
    ;(log/trace "config" kw (nil? @config-store) (some-> @config-store (get kw)))
   	(locking data-lock
      	(when (nil? @config-store)
	      	(try
	         	(load-config)
	          	(catch Exception e (load-default-config)))))
    (cond
      	(some? (get @config-store kw)) (get @config-store kw)
       	(= kw :horiz-res)       (horiz-res)
        (= kw :vert-res)        (vert-res)
        (= kw :graphics-height) (graphics-height)
        (= kw :graphics-width)  (graphics-width)
        (= kw :clock-height)    (clock-height)
        (= kw :clock-width)     (clock-width)
        (= kw :radar-width)     (radar-width)
        (= kw :radar-height)    (radar-height)
        (= kw :wnow-height)     (wnow-height)
        (= kw :wnow-width)      (wnow-width)
        ))

;;-----------------------------------------------------------------------------

