(ns smhi.date
  	(:require 	(smhi 			[utils  :as utils])
            	(clojure.spec 	[alpha  :as s])
            	(clojure 		[string :as str])
            	(clj-time 		[core   :as t]
            					[format :as f]
            					[local  :as l])
            	(taoensso 		[timbre :as log])))

;;-----------------------------------------------------------------------------

(s/def :dates/cachetid 		#(instance? org.joda.time.DateTime %))
(s/def :dates/version 		#(= % "2.1"))
(s/def :dates/uri 			string?)
(s/def :dates/startdatum 	#(instance? org.joda.time.DateTime %))
(s/def :dates/slutdatum 	#(instance? org.joda.time.DateTime %))
(s/def :dates/dagar 		(s/coll-of :dates/dag))
(s/def :dates/dag 			(s/keys :req-un [:dates/datum    :dates/veckodag :dates/arbetsfri
								   			 :dates/redday   :dates/namnsdag :dates/vecka
								   			 :dates/flaggdag :dates/daynum]
						  			:opt-un [:dates/helgdag]))
(s/def :dates/datum 		#(instance? org.joda.time.DateTime %))
(s/def :dates/veckodag 		string?)
(s/def :dates/arbetsfri 	boolean?)
(s/def :dates/redday 		boolean?)
(s/def :dates/namnsdag 		(s/coll-of string?))
(s/def :dates/vecka 		int?)
(s/def :dates/helgdag 		string?)
(s/def :dates/flaggdag 		string?)
(s/def :dates/daynum 		int?)
(s/def :dates/data 			(s/keys :req-un [:dates/cachetid :dates/version :dates/uri
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
		(= v "Ja")  			 true
		(= v "Nej") 			 false
		(try-parse :date v) 	 (f/parse (f/formatters :date) v)
		(try-parse :mysql v) 	 (f/parse (f/formatters :mysql) v)
		:else 					 v))

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
	(log/info "getting new date info")
	(try
		(let [url (str "http://api.dryg.net/dagar/v2.1/" (t/year ymd)
						"/" (t/month ymd) "/" (t/day ymd))
			  response (trans-map (utils/send-json-request url cleaner))]
			(log/info "successfully got new date info")
			(if-not (s/valid? :dates/data response)
	            (do
	                (log/error (str "\n------- Invalid date data -----------\n"
	                			 (s/explain-str :dates/data response) "\n"
	                			"-------------------------------------"))
	                nil)
				response))
		(catch Exception e
            (do
                (log/error (str "\n---------------------------------------------------\n"
                			"Error in: send-date-request\n" e "\n"
                			"---------------------------------------------------"))
                nil))))

;;-----------------------------------------------------------------------------

(def date-data (atom {}))

;;-----------------------------------------------------------------------------

(defn get-date-info
	[target]
 	{:pre [(instance? org.joda.time.LocalDate target)]
     :post [(s/valid? :dates/dag %)]}
  	(log/trace "get-date-info")
	(if-let [data (get @date-data target)]
		data
		(let [new-data (send-date-request target)
			  new-day  (first (:dagar new-data))]
			(swap! date-data (fn [x] (assoc x target new-day)))
			new-day)))

;;-----------------------------------------------------------------------------

(defn dt->d
	[target]
 	{:pre [(instance? org.joda.time.DateTime target)]
     :post [(instance? org.joda.time.LocalDate %)]}
    (log/trace "dt->d")
	(t/local-date (t/year target) (t/month target) (t/day target)))

;;-----------------------------------------------------------------------------

(defn red-day?
	[target]
 	{:pre [(instance? org.joda.time.DateTime target)]
     :post [(boolean? %)]}
	(try
		(-> target dt->d get-date-info :redday)
		(catch Exception e
			(do
     			(log/trace "red-day? exception" e)
     			false))))

;;-----------------------------------------------------------------------------
