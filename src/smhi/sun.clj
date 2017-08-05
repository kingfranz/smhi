(ns smhi.sun
  (:require (smhi 			[utils      :refer :all]
            				[spec       :refer :all]
            				[config     :refer [config]])
            (clojure.data 	[json       :as json])
            (clojure.java 	[io         :as io])
            (clojure.spec 	[alpha      :as s])
            (clj-time 		[core       :as t]
            				[format     :as f]
            				[local      :as l])
            (seesaw 		[core       :as sc]
            				[border     :as sb]
            				[graphics 	:as sg]
            				[color      :as sclr]
            				[font       :as sf])
            (org.httpkit 	[client     :as http])
            (taoensso 		[timbre     :as log])))

;;-----------------------------------------------------------------------------

(def sun-info (atom nil))

;;-----------------------------------------------------------------------------

(defn pp-sun
    [sun f]
    (let [t-str  (fn [x] (->> sun :results x f/parse hour-minute))]
        (f (str "astronomical_twilight_begin: " (t-str :astronomical_twilight_begin)))
        (f (str "nautical_twilight_begin:     " (t-str :nautical_twilight_begin)))
        (f (str "civil_twilight_begin:        " (t-str :civil_twilight_begin)))
        (f (str "sunrise:                     " (t-str :sunrise)))
        (f (str "solar_noon:                  " (t-str :solar_noon)))
        (f (str "day_length:                  " (format "%.1f" (float (/ (->> sun :results :day_length) 3600)))))
        (f (str "sunset:                      " (t-str :sunset)))
        (f (str "civil_twilight_end:          " (t-str :civil_twilight_end)))
        (f (str "nautical_twilight_end:       " (t-str :nautical_twilight_end)))
        (f (str "astronomical_twilight_end:   " (t-str :astronomical_twilight_end)))
    ))

;;-----------------------------------------------------------------------------

(defn send-sun-request
	[]
	(log/info "getting new sun info")
	(try
		(let [url (str "http://api.sunrise-sunset.org/json?lat=" (config :latitude)
					   "&lng=" (config :longitude) "&formatted=0")
			  response (send-json-request url)]
			(log/info "successfully got new sun info")
			(if-not (s/valid? :sun/sunrise-spec response)
	            (do
	                (log/error (str "\n------- Invalid Sunrise data -----------\n"
	                				(s/explain-str :sun/sunrise-spec response) "\n"
	                				"-------------------------------------"))
	                nil)
				(assoc response :timestamp (l/local-now))))
		(catch Exception e
            (do
                (log/error (str "\n---------------------------------------------------\n"
                				"Error in: send-sun-request\n"
                				"Exception: " (.getMessage e) "\n"
                				"---------------------------------------------------"))
                nil))))

(defn get-sun-info
	[]
	(if (or (nil? @sun-info) (> (t/in-minutes (t/interval (:timestamp @sun-info) (l/local-now))) (* 12 60)))
		(reset! sun-info (send-sun-request)))
	@sun-info)

(defn write-sun-info
	[^java.awt.Graphics2D g2d width height]
	(if-let [sun-info (get-sun-info)]
		(let [rise-txt      (->> sun-info :results :sunrise f/parse hour-minute)
			  set-txt       (->> sun-info :results :sunset f/parse hour-minute)
			  up-down-txt   (format "↑ %s  ↓ %s" rise-txt set-txt)
     		  up-down-width (string-width g2d (config :sun-txt-style) up-down-txt)
         	  up-down-x     (- (half width) (half up-down-width))
			  week-txt      (str "week: " (t/week-number-of-year (l/local-now)))
     		  week-width    (string-width g2d (config :sun-txt-style) week-txt)
         	  week-x        (- (half width) (half week-width))
              date-txt      (f/unparse (f/formatter "EEEE dd MMM") (l/local-now))
              date-width    (string-width g2d (config :sun-txt-style) date-txt)
              date-x        (- (half width) (half date-width))
              text-height   (string-height g2d (config :sun-txt-style))
              mid-y         (half height)]
			(sg/draw g2d
				(sg/rounded-rect (- up-down-x (half (config :sun-box-dw)))
                  		  		 (- mid-y (config :up-down-dy) (config :sun-box-dy))
                      	  		 (+ up-down-width (config :sun-box-dw))
                  		  		 (+ text-height (config :sun-box-dh))
                  		  		 (config :sun-box-radius)
                          		 (config :sun-box-radius))
				(config :sun-box-style))
			(sg/draw g2d
    	  		(sg/string-shape up-down-x
                          		 (- mid-y (config :up-down-dy))
                		   		 up-down-txt)
    	  		(config :sun-txt-style))
   
			(sg/draw g2d
				(sg/rounded-rect (- date-x (half (config :sun-box-dw)))
                  		  		 (- mid-y (config :date-dy) (config :sun-box-dy))
                      	  		 (+ date-width (config :sun-box-dw))
                  		  		 (+ text-height (config :sun-box-dh))
                  		  		 (config :sun-box-radius)
                          		 (config :sun-box-radius))
				(config :sun-box-style))
			(sg/draw g2d
    	  		(sg/string-shape date-x
                          		 (- mid-y (config :date-dy))
                		   		 date-txt)
    	  		(config :sun-txt-style))
   
			(sg/draw g2d
				(sg/rounded-rect (- week-x (half (config :sun-box-dw)))
                  		  		 (- mid-y (config :week-dy) (config :sun-box-dy))
                      	  		 (+ week-width (config :sun-box-dw))
                  		  		 (+ text-height (config :sun-box-dh))
                  		  		 (config :sun-box-radius)
                          		 (config :sun-box-radius))
				(config :sun-box-style))
			(sg/draw g2d
    	  		(sg/string-shape week-x
                          		 (- mid-y (config :week-dy))
                		   		 week-txt)
    	  		(config :sun-txt-style)))))

(defn draw-sunrise
  	"draw axises for forecast graphics (and scales)"
    [^java.awt.Graphics2D g2d width height]
  	(when-let [info (get-sun-info)]
        (let [sec->x               (fn [s] (* (/ width (* 24 60 60)) s))
              dt->sec              (fn [x] (+ (* (t/hour x) 60 60) (* (t/minute x) 60) (t/second x)))
              twilight-start-x     (-> info :results (get (config :twilight-begin)) f/parse dt->sec sec->x int)
              sunrise-x            (-> info :results :sunrise f/parse dt->sec sec->x int)
              sunset-x             (-> info :results :sunset f/parse dt->sec sec->x int)
              twilight-end-x       (-> info :results (get (config :twilight-end)) f/parse dt->sec sec->x int)
              twilight-start-width (- sunrise-x twilight-start-x)
              twilight-end-width   (- twilight-end-x sunset-x)
              mk-gradient          (fn [xs w l]
              	(sg/style :background
                    (sg/linear-gradient :start [xs 0]
                                     	:end [(+ xs w) 0]
                                     	:colors [(sclr/color 30 30 30 (if l 255 0))
                                              	 (sclr/color 30 30 30 (if l 0 255))])))]
            (sg/draw g2d
                (sg/rect 0 0 twilight-start-x height)
                (sg/style :background (sclr/color 30 30 30)))
            (sg/draw g2d
                (sg/rect twilight-start-x 0 twilight-start-width height)
                (mk-gradient twilight-start-x twilight-start-width true))
            (sg/draw g2d
                (sg/rect sunset-x 0 twilight-end-width height)
                (mk-gradient sunset-x twilight-end-width false))
            (sg/draw g2d
                (sg/rect twilight-end-x 0 (- width twilight-end-x) height)
                (sg/style :background (sclr/color 30 30 30))))))

;;-----------------------------------------------------------------------------
