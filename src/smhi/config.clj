(ns smhi.config
	(:require 	(clojure.tools.reader 	[edn      :as edn])
				(seesaw 				[core     :as sc]
				 						[border   :as sb]
				 						[graphics :as sg]
				 						[color    :as sclr]
				 						[font     :as sf])
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

(def ^:private horizontal-resolution 1920)
(def ^:private vertical-resolution   1080)

(def ^:private default-config
	{
	:radar-sub-width            112
	:radar-sub-height           45
	:radar-sub-upper-left-x     110
	:radar-sub-upper-left-y     595
	:radar-timer-initial-sec    0
	:radar-interval-minutes     5
	:radar-fps                  10
	:radar-ani-hours            5
	:radar-ani-delay-sec        6
	:radar-url                  "http://opendata-download-radar.smhi.se/api/version/latest/area/sweden/product/comp/latest.png"
	:radar-txt-style            (sg/style :foreground :black :font "ARIAL-BOLD-64")
    :radar-border-size          10
    :radar-txt-x                220
    :radar-txt-y                140
	:radar-height               (* (- vertical-resolution (* vertical-resolution 1/3)) 2/3)
	:radar-width                (- horizontal-resolution (- vertical-resolution (* vertical-resolution 1/3)))
 	:radar-width-long		    3.2
   
 	:clock-fps                  1
  	:clock-delay-sec            6
	:clock-height               (- vertical-resolution (* vertical-resolution 1/3))
	:clock-width                (- vertical-resolution (* vertical-resolution 1/3))
    
  	:weather-timer-initial-sec  0
	:weather-timer-delay-min    30
	:wind-style                 (sg/style :foreground :white :background :lightgray :stroke 1)
	:rain-style                 (sg/style :foreground :blue :background :blue :stroke 1)
	:temp-style                 (sg/style :foreground :red :background :red :stroke 8)
	:cloud-style                (sg/style :foreground :green :background :lightgreen :stroke 8)
	:axis-style                 (sg/style :foreground :white :background :white :stroke 2)
	:day-tick-style             (sg/style :foreground :white :background :white :stroke 1)
	:temp-axis-text-style       (sg/style :foreground :red :background :red :stroke 1 :font "ARIAL-BOLD-20")
	:wind-axis-text-style       (sg/style :foreground :grey :background :grey :stroke 1 :font "ARIAL-BOLD-20")
	:rain-axis-text-style       (sg/style :foreground :blue :background :blue :stroke 1 :font "ARIAL-BOLD-20")
	:zero-line-style            (sg/style :foreground :white :background :white :stroke 1)
	:text-circle-style          (sg/style :foreground :white :background :white :stroke 1)
	:min-fixed-temp             -20
	:max-fixed-temp             30
	:image-dir                  "resources/"
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
	:wnow-title-style           (sg/style :foreground :white :font "ARIAL-18")
	:wnow-value-style           (sg/style :foreground :white :font "ARIAL-48")
	:info-bg-style              (sg/style :foreground (sclr/color 32 32 32) :stroke 2
							     		 :background (sclr/color 128 128 128 128))
	:date-txt-style             (sg/style :foreground :white :background :black :stroke 2 :font "ARIAL-192")
	:exception-style            (sg/style :foreground :red   :font "ARIAL-64")
	:graph-days                 7
	:axis-width                 2
 	:wnow-vt-adjust             -3
 	:wnow-vv-adjust             -19
  	:forecast-bg		        (sclr/color 128 128 128 128)
   	:day-font                   (sf/font "ARIAL-192")
    :day-stroke-width           3
    :symbols-per-day            3
     
	:horiz-res                  horizontal-resolution
	:vert-rez                   vertical-resolution
	:graphics-height            (* vertical-resolution 1/3)
	:graphics-width             horizontal-resolution
	:info-height                (- (- vertical-resolution (* vertical-resolution 1/3)) (* (- vertical-resolution (* vertical-resolution 1/3)) 2/3))
	:info-width                 (- horizontal-resolution (- vertical-resolution (* vertical-resolution 1/3)))
	:wnow-height                (/ (- (- vertical-resolution (* vertical-resolution 1/3)) (* (- vertical-resolution (* vertical-resolution 1/3)) 2/3)) 2)
	:wnow-width                 (/ (- horizontal-resolution (- vertical-resolution (* vertical-resolution 1/3))) 5)
	 
 	:sun-box-dw                 50
 	:sun-box-dh                 15
 	:sun-box-dy                 40
 	:sun-box-radius             50
	:sun-txt-style              (sg/style :foreground :black :font "ARIAL-BOLD-36")
	:sun-box-style              (sg/style :foreground :black :stroke 2 :background (sclr/color 140 140 140))
	:up-down-dy                 100
	:date-dy                    -100
	:week-dy                    -180
 	 
	 
 	:left-axis-width            100
	:right-axis-width           50
     
	:temp-padding               5
	:wind-padding               2
	:tick-width                 10
	:temp-text-x                (- 100 10 3)
	:wind-text-x                (- (/ 100 2) 10 3)
	:axis-span                  10
	:wind-axis-factor           3
	:smhi-timeout               5000
	:twilight-begin             :civil_twilight_begin
	:twilight-end               :civil_twilight_end
	:wnow-top-border            9
    :wnow-center-border         6
    :wnow-bottom-border         14
    :wnow-side-border           10
    :wnow-title-part            1/3
    :wnow-radius                10
    }) 

;;-----------------------------------------------------------------------------

(def ^:private config-store (atom default-config))

;;-----------------------------------------------------------------------------

(defn config
  	"retrive a config value"
  	[kw]
    {:pre [(keyword? kw)]}
   	(when (nil? (get @config-store kw))
      	(throw (ex-info (str "config: unknown key: " kw) {:cause :keyword})))
    (get @config-store kw))

(defn week-minutes
	[]
	(* (config :graph-days) 24 60))

(defn rain-axis-span
	[]
	(config :max-rain-level))

(defn radar-interval-ms
	[]
	(* (config :radar-interval-minutes) 60 1000))

(defn max-radar-queue-size
	[]
	(* (/ 60 (config :radar-interval-minutes)) (config :radar-ani-hours)))

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

(defn read-config-file
	[]
 	(spit config-name (with-out-str (prn (convert-config default-config))))
	(some->> config-name
             slurp
             (edn/read-string {:readers {'smhi.config.SMHIStyle create-style
                                         'smhi.config.SMHIColor create-color
                                         'smhi.config.SMHIFont  create-font}})
             (reset! config-store)
             ))

;;-----------------------------------------------------------------------------

