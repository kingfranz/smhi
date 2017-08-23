(ns smhi.config
	(:require 	(clojure.tools.reader 	[edn           :as edn])
				(seesaw 				[core          :as sc]
				 						[border        :as sb]
				 						[graphics      :as sg]
				 						[color         :as sclr]
				 						[font          :as sf])
            	(clojure.java 			[io            :as io])
            	(clojure.math 			[numeric-tower :as math])
                (taoensso               [timbre        :as log])
                (clojure-watch          [core          :refer [start-watch]])
    			(clojure				[pprint        :as pp]
                    					[string        :as str])))

;;-----------------------------------------------------------------------------

(def config-name "./smhi-config.edn")

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

(def ^:private default-config
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
	:wind-style                 (with-meta {:foreground :white :background :lightgray :stroke 1} {:seesaw :style})
	:rain-style                 (with-meta {:foreground :blue :background :red :stroke 1} {:seesaw :style})
	:temp-style                 (with-meta {:foreground :red :background :red :stroke 8} {:seesaw :style})
	:cloud-style                (with-meta {:foreground :lightgreen :background :lightgreen :stroke 10} {:seesaw :style})
	:axis-style                 (with-meta {:foreground :white :background :white :stroke 2} {:seesaw :style})
	:day-tick-style             (with-meta {:foreground :white :background :white :stroke 1} {:seesaw :style})
	:zero-line-style            (with-meta {:foreground :white :background :white :stroke 1} {:seesaw :style})
	:text-circle-style          (with-meta {:foreground :white :background :white :stroke 1} {:seesaw :style})
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
  
	:widget-style               (with-meta [32 32 32 0] {:seesaw :color})
	:degree-char                "\u00b0"
	:max-rain-level             3
 	:right-axis-extra           30
	:info-bg-style              (with-meta {:foreground (with-meta [32 32 32] {:seesaw :color})
                                            :stroke 2
							     		    :background (with-meta [128 128 128 128] {:seesaw :color})} {:seesaw :style})
	:graph-days                 7
	:axis-width                 2
 	:wnow-vt-adjust             -3
 	:wnow-vv-adjust             -19
  	:forecast-bg		        (with-meta [128 128 128 128] {:seesaw :color})
    :day-stroke-width           3
    :symbols-per-day            3
    :day-outline-style          (with-meta {:foreground :white
                                            :stroke 3
                                            :font "ARIAL" :fontsz 192} {:seesaw :style})
    :day-style                  (with-meta {:foreground :black
                                            :stroke 3
                                            :font "ARIAL" :fontsz 192} {:seesaw :style})
    :day-red-style              (with-meta {:foreground :red
                                            :stroke 3
                                            :font "ARIAL" :fontsz 192} {:seesaw :style})
     
	:sun-box-style              (with-meta {:foreground :black
                                            :stroke 2
                                            :background (with-meta [140 140 140] {:seesaw :color})} {:seesaw :style})
     
	:temp-padding               5
	:wind-padding               2
	:axis-span                  10
	:wind-axis-factor           3
	:smhi-timeout               5000
	:twilight-begin             :civil_twilight_begin
	:twilight-end               :civil_twilight_end
    :wnow-title-part            1/3
    
    :clock-switch-sec           10
    :bar-width-percent			0.80
    :bar-height-percent			0.10
    :date-str-y					0.23
    :time-str-y					0.37
    :today-bar-y				0.40
    :month-bar-y				0.60
    :year-bar-y					0.80
    :bar-style					(with-meta {:foreground :grey
                               				:stroke 4
                                   			:background (with-meta [140 140 140 50] {:seesaw :color})} {:seesaw :style})

	:radar-txt-style      		(with-meta {:foreground :black
                                     		:font "ARIAL-BOLD" :fontsz 64} {:seesaw :style})
	:temp-axis-text-style 		(with-meta {:foreground :red
                                     		:background :red
                                       		:stroke 1
                                         	:font "ARIAL-BOLD" :fontsz 20} {:seesaw :style})
	:wind-axis-text-style 		(with-meta {:foreground :grey
                                     		:background :grey
                                       		:stroke 1
                                         	:font "ARIAL-BOLD" :fontsz 20} {:seesaw :style})
	:rain-axis-text-style 		(with-meta {:foreground :blue
                                     		:background :blue
                                       		:stroke 1
                                         	:font "ARIAL-BOLD" :fontsz 20} {:seesaw :style})
	:wnow-title-style     		(with-meta {:foreground :white
                                     		:font "ARIAL" :fontsz 18} {:seesaw :style})
	:wnow-value-style     		(with-meta {:foreground :white
                                     		:font "ARIAL" :fontsz 48} {:seesaw :style})
	:date-txt-style       		(with-meta {:foreground :white
                                     		:background :black
                                       		:stroke 2
                                         	:font "ARIAL" :fontsz 192} {:seesaw :style})
	:exception-style      		(with-meta {:foreground :red
                                     		:font "ARIAL" :fontsz 64} {:seesaw :style})
   	:sun-txt-style        		(with-meta {:foreground :black
                                        	:font "ARIAL" :fontsz 36} {:seesaw :style})
    :date-stroke-style    		(with-meta {:foreground :black
                                        	:font "ARIAL-BOLD" :fontsz 80 :stroke 1} {:seesaw :style})
    :date-fill-style      		(with-meta {:foreground :white
                                        	:font "ARIAL-BOLD" :fontsz 80 :stroke 1} {:seesaw :style})
    :bar-stroke-style     		(with-meta {:foreground :black
                                        	:font "ARIAL-BOLD" :fontsz 64 :stroke 1} {:seesaw :style})
  	:bar-fill-style       		(with-meta {:foreground :white
                                       		:font "ARIAL-BOLD" :fontsz 64 :stroke 1} {:seesaw :style})

    :axis-radius 		 		(with-meta {:value 17}   {:dir :both})
    :radar-border-size  		(with-meta {:value 10}   {:dir :both})
    :radar-txt-x        		(with-meta {:value 20}   {:dir :horizontal})
    :radar-txt-y        		(with-meta {:value 20}   {:dir :vertical})
	:sun-box-dw         		(with-meta {:value 50}   {:dir :horizontal})
 	:sun-box-dh         		(with-meta {:value 15}   {:dir :vertical})
 	:sun-box-dy         		(with-meta {:value 40}   {:dir :vertical})
 	:sun-box-radius     		(with-meta {:value 50}   {:dir :both})
	:up-down-dy         		(with-meta {:value 100}  {:dir :vertical})
	:date-dy            		(with-meta {:value -100} {:dir :vertical})
	:week-dy            		(with-meta {:value -180} {:dir :vertical})
 	:left-axis-width    		(with-meta {:value 100}  {:dir :horizontal})
	:right-axis-width   		(with-meta {:value 50}   {:dir :horizontal})
	:tick-width         		(with-meta {:value 15}   {:dir :horizontal})
	:wnow-top-border    		(with-meta {:value 6}    {:dir :vertical})
    :wnow-center-border 		(with-meta {:value 5}    {:dir :vertical})
    :wnow-bottom-border 		(with-meta {:value 10}   {:dir :vertical})
    :wnow-side-border   		(with-meta {:value 8}   {:dir :horizontal})
    :wnow-radius        		(with-meta {:value 10}   {:dir :both})
    :small-symbol-sz    		(with-meta {:value 50}   {:dir :both})
    })

;;-----------------------------------------------------------------------------

(def ^:private config-store (atom nil))

;;-----------------------------------------------------------------------------

(defn- horiz-res       [] (math/round (* std-horizontal-res (:horizontal-scale @config-store))))
(defn- vert-res        [] (math/round (* std-vertical-res (:vertical-scale @config-store))))
(defn- graphics-width  [] (math/round (horiz-res)))
(defn- graphics-height [] (math/round (* (vert-res) 1/3)))
(defn- clock-width     [] (math/round (* (vert-res) 2/3)))
(defn- clock-height    [] (math/round (* (vert-res) 2/3)))
(defn- radar-width     [] (math/round (- (horiz-res) (clock-width))))
(defn- radar-height    [] (math/round (* (clock-height) 2/3)))
(defn- wnow-width      [] (math/round (/ (radar-width) 5)))
(defn- wnow-height     [] (math/round (/ (- (vert-res) (graphics-height) (radar-height)) 2)))

;;-----------------------------------------------------------------------------

(defn- write-config
  	[]
   	(log/trace "write-config ENTER")
   	(let [longest (->> default-config
                       keys
                       (map str)
                       (map count)
                       (apply max))
          mk-str (fn [kv] (as-> kv $
                                (key $)
                                (str $)
                                (count $)
                                (- longest $)
                                (+ 2 $)
                                (repeat $ " ")
                                (str/join $)))]
      	(binding [*print-meta* true]
         	(with-open [w (clojure.java.io/writer config-name)]
        		(.write w "{\n")
        		(doseq [kv (sort-by first (seq default-config))]
          			(.write w (str "    " (key kv) (mk-str kv) (pr-str (val kv)) "\n")))
         		(.write w "}\n")))
    	(log/trace "write-config EXIT")))

(defn- scale-value
  	[v hscale vscale bscale]
   	(if (nil? (meta v))
      	v
      	(cond
         	(= (:dir (meta v)) :vertical)
         		(* (:value v) vscale)
          	(= (:dir (meta v)) :horizontal)
         		(* (:value v) hscale)
          	(= (:dir (meta v)) :both)
         		(* (:value v) bscale)
          	(= (:seesaw (meta v)) :style)
           		(sg/style :foreground (some-> (:foreground v) (scale-value hscale vscale bscale))
                       	  :background (some-> (:background v) (scale-value hscale vscale bscale))
                          :stroke     (when (some? (:stroke v)) (* (:stroke v) bscale))
                          :font       (when (and (some? (:font v)) (some? (:fontsz v)))
                                          (str (:font v) "-" (math/round (* (:fontsz v) bscale)))))
            (= (:seesaw (meta v)) :color)
           		(apply sclr/color v)
            :else (throw (ex-info (str "unknown meta " (meta v)) {:cause :unknown-meta})))))

(defn- scale-config
  	[conf]
    (let [hscale (:horizontal-scale conf)
          vscale (:vertical-scale conf)
          bscale (/ (math/sqrt (+ hscale vscale)) (math/sqrt 2.0))
          conf*  (into {} (map (fn [[k v]] [k (scale-value v hscale vscale bscale)]) conf))]
        (reset! config-store conf*)))

(defn- load-config
  	[]
    (log/trace "load-config ENTER")
   	(scale-config (some->> config-name slurp edn/read-string))
    (log/trace "load-config EXIT"))

(defn- load-default-config
  	[]
   	(log/trace "load-default-config ENTER")
   	(scale-config default-config)
    (log/trace "load-default-config EXIT"))

(defn- setup-config
    []
    (log/info "starting watch")
    (start-watch [{
        :path "./"
        :event-types [:modify]
        :bootstrap (fn [path]
            (try
	         	(when-not (.exists (io/as-file config-name))
                    (write-config))
   				(load-config)
	          	(catch Exception _ (load-default-config))))
        :callback (fn [event filename]
            (when (= filename config-name)
              	(try
	         		(load-config)
	          		(catch Exception _ (load-default-config)))))
        :options {:recursive false}}])
    (log/info "after watch"))

(def ^:private data-lock (Object.))

(defn config
  	"retrive a config value"
  	[kw]
    {:pre [(keyword? kw)]}
    ;(log/trace "config" kw (nil? @config-store) (some-> @config-store (get kw)))
   	(locking data-lock
      	(when (nil? @config-store)
	      	(setup-config)))
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

