(ns smhi.weather
    (:require 	(smhi 			[utils    :refer :all]
                     			[spec     :refer :all]
                     			[config   :refer [config units week-minutes]])
              	(clojure.spec 	[alpha    :as s])
                (clojure        [pprint   :as pp])
            	(clj-time 		[core     :as t]
              					[format   :as f]
              					[coerce   :as c]
              					[local    :as l])
              	(seesaw 		[timer    :as st]
              					[core     :as sc]
              					[border   :as sb]
              					[graphics :as sg]
              					[color    :as sclr]
              					[font     :as sf]
              					[dev      :as sd])
              	(taoensso 		[timbre   :as log])))

;;-----------------------------------------------------------------------------

; the current weather forecast
(def weather-data (atom nil))

; not nil if we got an exception
(def weather-exception (atom nil))

(defn filter-param
    "filter SMHI data based on one specific unit"
    [param units]
    ;(log/info "enter: filter-param")
    (let [param-key (keyword (:name param))]
        (if (-> units param-key :keep)
            {param-key
             (if (-> units param-key :is-int)
                 (-> param :values first (+ 0.5) long)
                 (-> param :values first))}
            {})))

(defn filter-param-map
    "filter SMHI data based on the units map"
    [params units]
    ;(log/info "enter: filter-param-map")
    (apply merge (map #(filter-param % units) params)))

;;-----------------------------------------------------------------------------

(defn mk-weather-url
    []
    (log/info "enter: mk-weather-url")
    (str (config :weather-url)
     "/api/category/"      (config :category)
     "/version/"           (config :version)
     "/geotype/point/lon/" (config :longitude)
     "/lat/"               (config :latitude)
     "/data.json"))

(defn send-weather-request
    "send a weather forecast request to SMHI"
    []
    (log/info "enter: send-weather-request")
    (let [forecast (send-json-request (mk-weather-url))]
        (log/info "send-weather-request: successfully got new forecast")
	    (if-not (s/valid? :smhi/smhi-spec forecast)
            (do
                (log/error
                  	(str "------- Invalid SMHI data -----------"
                		 (s/explain-str :smhi/smhi-spec forecast)
                		 "-------------------------------------"
                		 forecast))
                (throw (Exception. "Invalid SMHI data"))))
        forecast))

;;-----------------------------------------------------------------------------

(defn interpolate
    "interpolate a Y value between 2 X,Y pairs"
    [x1 y1 x2 y2 target-x is-int?]
    (let [k        (/ (- y2 y1) (- x2 x1))
          m        (- y1 (* k x1))
          target-y (+ (* k target-x) m)]
        (if is-int?
            (long target-y)
            target-y)))

(defn mk-intermediate
    "create a new entry between before and after"
    [before after target-minute target-px]
    (log/info "enter: mk-intermediate")
    {:minutes-x target-minute
     :pixels-x  target-px
     :timestamp (c/from-long (interpolate (:minutes-x before)
                             (c/to-long (:timestamp before))
                             (:minutes-x after)
                             (c/to-long (:timestamp after))
                             target-minute
                             true))
     :values    (into {} (map (fn [k] [k (interpolate (:minutes-x before)
                                                      (-> before :values k)
                                                      (:minutes-x after)
                                                      (-> after :values k)
                                                      target-minute
                                                      (-> units k :is-int))])
                              (keys (:values before))))})

(defn mk-delta-time
    "convert the SMHI timestamp to minutes from midnight (or nil if it's too old)"
    [m]
    ;(log/info "enter: mk-delta-time")
    (when (t/after? (f/parse (:validTime m)) (t/now))
        (t/in-minutes (t/interval (t/today-at 00 00) (f/parse (:validTime m))))))

(defn dump
  	[xx]
    (spit "forecast-raw.edn" (with-out-str (pp/pprint xx)))
    xx)

(defn process-data
    "process the SMHI data and convert timestamp to delta minutes"
    [width height]
    (log/info "enter: process-data")
    (let [px-per-min (/ width (week-minutes))
          valid-resp (some->> (send-weather-request)
      	          			 :timeSeries
       						 (map (fn [x] {:timestamp (f/parse (:validTime x))
                            			   :minutes-x (mk-delta-time x)
                            			   :pixels-x  (some-> (mk-delta-time x) (* px-per-min) int)
                            			   :values    (filter-param-map (:parameters x) units)}))
      					     (remove #(nil? (:minutes-x %)))) ; remove old ones
          week-resp (remove #(>= (:minutes-x %) (week-minutes)) valid-resp)] ; and only this week
        (if (> (count valid-resp) (count week-resp))
            (let [next-entry (nth valid-resp (count week-resp))
                  prev-entry (last week-resp)
                  new-entry  (mk-intermediate prev-entry next-entry (dec (week-minutes)) (dec width))]
                (conj week-resp new-entry))
            week-resp)))

(def weather-timer-ts (atom nil))

(defn time-for-forecast?
    ""
    []
    ;(log/info "enter: time-for-forecast?")
    (or (nil? @weather-timer-ts)
        (> (t/in-minutes (t/interval @weather-timer-ts (l/local-now))) 30)))

(defn weather-update
  	[width height]
    ;(log/info "enter: weather-update")
    (try
	   	(if (time-for-forecast?)
	        (do
	            (log/info "getting new forecast")
	            (reset! weather-data (sort-by :minutes-x (process-data width height)))
	            (reset! weather-exception nil)
	            (reset! weather-timer-ts (l/local-now))
	            (log/info "weather-timer-fn: done!")
             	(spit "forecast-post.edn" (with-out-str (pp/pprint @weather-data)))
             	:new)
         	:old)
        (catch Exception e
            (reset! weather-exception e)
            (reset! weather-timer-ts nil)
            (log/error (str
                 "\n---------------------------------------------------\n"
    			 "Exception in weather-timer-fn: " (.getMessage e) "\n" e "\n"
    			 "---------------------------------------------------"))
            (if (nil? @weather-data)
              	:none
                :old))))
