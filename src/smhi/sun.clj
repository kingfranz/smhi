(ns smhi.sun
    (:require [smhi.config        :refer :all])
    (:require [smhi.utils        :refer :all])
    (:require [smhi.spec        :refer :all])
    (:require [clojure.data.json          :as json])
    (:require [clojure.java.io            :as io])
    (:require [clojure.spec               :as s])
    (:require [clojure.string             :as str])
    (:require [clj-time.core              :as t])
    (:require [clj-time.format            :as f])
    (:require [clj-time.local             :as l])
    (:use seesaw.core)
	(:use seesaw.border)
	(:use seesaw.graphics)
	(:use seesaw.color)
	(:use seesaw.font)
	(:require [taoensso.timbre            :as timbre
               :refer [log  trace  debug  info  warn  error  fatal  report
                       logf tracef debugf infof warnf errorf fatalf reportf spy get-env]])
    (:require [taoensso.timbre.appenders.core :as appenders])
    (:import (javax.swing JFrame JLabel)
             (java.awt Color Font FontMetrics GraphicsEnvironment)
             (java.io ByteArrayInputStream)))


(def sun-info (atom nil))


(defn send-sun-request
	[config]
	(info "getting new sun info")
	(try
		(let [url (str "http://api.sunrise-sunset.org/json?lat=" (:latitude config)
					   "&lng=" (:longitude config) "&formatted=0")
			  response (send-json-request url)]
			(info "successfully got new sun info")
			(if (= (s/conform sunrise-spec response) :clojure.spec/invalid)
	            (do
	                (error "------- Invalid Sunrise data -----------")
	                (error (s/explain-str sunrise-spec response))
	                (error "-------------------------------------")
	                nil)
				(assoc response :timestamp (l/local-now))))
		(catch Exception e
            (do
                (error "---------------------------------------------------")
                (error "Error in: send-sun-request")
                (error (str "Exception: " (.getMessage e)))
                (error "---------------------------------------------------")
                nil))))

(defn get-sun-info
	[]
	(if (or (nil? @sun-info) (> (t/in-minutes (t/interval (:timestamp @sun-info) (l/local-now))) (* 12 60)))
		(set-var sun-info (send-sun-request smhi-config)))
	@sun-info)

(defn inprint-image
	[image]
	(if-let [sun-info (get-sun-info)]
		(let [g2d (.createGraphics image)
			  to-txt (fn [x] (f/unparse (f/with-zone (f/formatter :hour-minute) (t/default-time-zone)) x))
			  rise-txt (->> sun-info :results :sunrise f/parse to-txt)
			  set-txt  (->> sun-info :results :sunset f/parse to-txt)
			  rise-width (string-width g2d sun-style rise-txt)
			  set-width (string-width g2d sun-style set-txt)
			  sq-width  200
			  sq-height 70
			  sq-radius 60]
			(draw g2d
    			(rounded-rect (- (:x sun-up-point) (/ sq-width 2))
            				  (- (:y sun-up-point) 50)
            				  sq-width
            				  sq-height
            				  sq-radius
            				  sq-radius)
    			sun-bg-style)
			(draw g2d
            	  (string-shape (- (:x sun-up-point) (/ rise-width 2))
                				(:y sun-up-point)
                        		rise-txt)
            	  sun-style)
			(draw g2d
    			(rounded-rect (- (:x sun-down-point) (/ sq-width 2))
            				  (- (:y sun-down-point) 50)
            				  sq-width
            				  sq-height
            				  sq-radius
            				  sq-radius)
    			sun-bg-style)
			(draw g2d
            	  (string-shape (- (:x sun-down-point) (/ set-width 2))
                				(:y sun-down-point)
                        		set-txt)
            	  sun-style)
			(.dispose g2d)))
	image)

