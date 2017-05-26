(ns smhi.date
  (:require (smhi 		[utils                 :as utils]
            			[spec                  :as spec]
            			[config                :as conf])
            [clojure.data.json          :as json]
            [clojure.java.io            :as io]
            (clojure 	[spec               :as s]
            			[string 		:as str])
            (clj-time 	[core              :as t]
            			[format            :as f]
            			[local             :as l])
            [org.httpkit.client         :as http]
            [taoensso.timbre            :as timbre]
            [taoensso.timbre.appenders.core :as appenders])
    (:import (javax.swing JFrame JLabel)
             (java.awt Color Font FontMetrics GraphicsEnvironment)
             (java.io ByteArrayInputStream)
             (java.lang.Math)))

;;-----------------------------------------------------------------------------

(defn current-year
	[]
	(t/year (l/local-now)))

(defn current-month
	[]
	(t/month (l/local-now)))

;;-----------------------------------------------------------------------------

(s/def :dates/cachetid #(instance? org.joda.time.DateTime %))
(s/def :dates/version #(= % "2.1"))
(s/def :dates/uri string?)
(s/def :dates/startdatum #(instance? org.joda.time.DateTime %))
(s/def :dates/slutdatum #(instance? org.joda.time.DateTime %))
(s/def :dates/dagar (s/coll-of :dates/dag))
(s/def :dates/dag (s/keys :req-un [:dates/datum :dates/veckodag :dates/arbetsfri
								   :dates/redday :dates/namnsdag :dates/vecka
								   :dates/flaggdag :dates/daynum]
						  :opt-un [:dates/helgdag]))
(s/def :dates/datum #(instance? org.joda.time.DateTime %))
(s/def :dates/veckodag string?)
(s/def :dates/arbetsfri boolean?)
(s/def :dates/redday boolean?)
(s/def :dates/namnsdag (s/coll-of string?))
(s/def :dates/vecka int?)
(s/def :dates/helgdag string?)
(s/def :dates/flaggdag string?)
(s/def :dates/daynum int?)
(s/def :dates/data (s/keys :req-un [:dates/cachetid :dates/version :dates/uri
									:dates/startdatum :dates/slutdatum :dates/dagar]))

;;-----------------------------------------------------------------------------

(defn cleaner
	[data]
	;(prn data)
	(-> data
		(str/replace "arbetsfri dag" "arbetsfri")
		(str/replace "r\\u00f6d dag" "redday")
		(str/replace "kl\\u00e4mdag" "betweenday")
		(str/replace "dag i vecka" "daynum")))

(defn update-map
	[m f]
	(reduce-kv (fn [m k v] (assoc m k (f v))) {} m))

(defn try-parse
	[frmt s]
	(try
		(f/parse (f/formatters frmt) s)
		true
		(catch Exception e
			false)))

(defn map-str
	[v]
	(cond
		(re-matches #"[0-9]+" v) (Integer/valueOf v)
		(= v "Ja")  true
		(= v "Nej") false
		(try-parse :date v) (f/parse (f/formatters :date) v)
		(try-parse :mysql v) (f/parse (f/formatters :mysql) v)
		:else v))

(defn trans-map
	[m]
	(update-map m (fn [v]
		(if (string? v)
			(map-str v)
			(if (and (coll? v) (map? (first v)))
				(map trans-map v)
				v)))))

(defn send-date-request
	[ymd]
	(timbre/info "getting new date info")
	(try
		(let [url (str "http://api.dryg.net/dagar/v2.1/" (t/year ymd)
						"/" (t/month ymd) "/" (t/day ymd))
			  response (trans-map (utils/send-json-request url cleaner))]
			(timbre/info "successfully got new date info")
			(if-not (s/valid? :dates/data response)
	            (do
	                (timbre/error "------- Invalid date data -----------")
	                (timbre/error (s/explain-str :dates/data response))
	                (timbre/error "-------------------------------------")
	                nil)
				response))
		(catch Exception e
            (do
                (timbre/error "---------------------------------------------------")
                (timbre/error "Error in: send-date-request")
                (timbre/error e)
                (timbre/error "---------------------------------------------------")
                nil))))

;;-----------------------------------------------------------------------------

(def date-data (atom {}))

;;-----------------------------------------------------------------------------

(defn date-limit
	[target]
	(let [limit-start (t/local-date)
		  limit-end   (t/plus limit-start (t/months 1))]
		(t/within? (t/interval limit-start limit-end) target)))

(s/fdef get-date-info
	:args #(and (instance? org.joda.time.LocalDate %)
				(date-limit %))
	:ret :dates/dag)

(defn get-date-info
	[target]
	(if-let [data (get @date-data target)]
		data
		(let [new-data (send-date-request target)
			  new-day  (first (:dagar new-data))]
			(swap! date-data (fn [x] (assoc x target new-day)))
			new-day)))

;;-----------------------------------------------------------------------------

(s/fdef dt->d
	:args #(instance? org.joda.time.DateTime %)
	:ret #(instance? org.joda.time.LocalDate %))

(defn dt->d
	[target]
	(t/local-date (t/year target) (t/month target) (t/day target)))

;;-----------------------------------------------------------------------------

(s/fdef red-day?
	:args #(instance? org.joda.time.DateTime %)
	:ret boolean?)

(defn red-day?
	[target]
	(try
		(:redday (get-date-info (dt->d target)))
		(catch Exception e
			false)))

;;-----------------------------------------------------------------------------
