(ns smhi.spec
    (:require 	(clojure.spec 	[alpha   :as s])
              	(clj-time 		[core    :as t]
              					[format  :as f]
              					[local   :as l])))

;;---------------------------------------------------------------------------------

(s/def :smhi/approvedTime   f/parse)
(s/def :smhi/referenceTime  f/parse)
(s/def :smhi/type           #(= % "Point"))
(s/def :smhi/coordinates    #(and (= (count %) 1)
                        		  (= (count (first %)) 2)
                        		  (-> % first first double?)
                        		  (-> % first second double?)))
(s/def :smhi/geometry      (s/keys :req-un [:smhi/type :smhi/coordinates]))
(s/def :smhi/timeSeries    (s/+ (s/keys :req-un [:smhi/validTime :smhi/parameters])))
(s/def :smhi/validTime     f/parse)

(s/def :smhi/parameters    (s/+ :smhi/param-entry))
(s/def :smhi/param-entry   (s/keys :req-un [:smhi/name
                                            :smhi/levelType
                                            :smhi/level
                                            :smhi/unit
                                            :smhi/values]))

(s/def :smhi/name          #(some #{%} #{"msl" "t" "vis" "wd" "ws"
                                         "r" "tstm" "tcc_mean" "lcc_mean"
                                         "mcc_mean" "hcc_mean" "gust" "pmin"
                                         "pmax" "spp" "pcat" "pmean" "pmedian" "Wsymb"}))
(s/def :smhi/levelType     #(some #{%} #{"hmsl" "hl"}))
(s/def :smhi/level         #(some #{%} #{0 2 10}))
(s/def :smhi/unit          #(some #{%} #{"Cel" "km" "degree" "m/s" "hPa"
                                         "percent" "octas" "kg/m2/h" "category"}))
(s/def :smhi/values        (s/and vector? #(= (count %) 1) #(number? (first %))))

(s/def :smhi/smhi-spec     (s/keys :req-un [:smhi/approvedTime
                                            :smhi/referenceTime
                                            :smhi/geometry
                                            :smhi/timeSeries]))

;;---------------------------------------------------------------------------------

(def sunrise-example
	{
		:results
		{
			:sunrise                     "2015-05-21T05:05:35+00:00"
			:sunset                      "2015-05-21T19:22:59+00:00"
			:solar_noon                  "2015-05-21T12:14:17+00:00"
			:day_length                  51444
			:civil_twilight_begin        "2015-05-21T04:36:17+00:00"
			:civil_twilight_end          "2015-05-21T19:52:17+00:00"
			:nautical_twilight_begin     "2015-05-21T04:00:13+00:00"
			:nautical_twilight_end       "2015-05-21T20:28:21+00:00"
			:astronomical_twilight_begin "2015-05-21T03:20:49+00:00"
			:astronomical_twilight_end   "2015-05-21T21:07:45+00:00"
		}
		:status "OK"
	})

(s/def :sun/status                      #(= % "OK"))
(s/def :sun/sunrise                     f/parse)
(s/def :sun/sunset                      f/parse)
(s/def :sun/solar_noon                  f/parse)
(s/def :sun/day_length                  (s/and integer? pos?))
(s/def :sun/civil_twilight_begin        f/parse)
(s/def :sun/civil_twilight_end          f/parse)
(s/def :sun/nautical_twilight_begin     f/parse)
(s/def :sun/nautical_twilight_end       f/parse)
(s/def :sun/astronomical_twilight_begin f/parse)
(s/def :sun/astronomical_twilight_end   f/parse)
(s/def :sun/results (s/keys :req-un [:sun/sunrise
									 :sun/sunset
									 :sun/solar_noon
									 :sun/day_length
									 :sun/civil_twilight_begin
									 :sun/civil_twilight_end
									 :sun/nautical_twilight_begin
									 :sun/nautical_twilight_end
									 :sun/astronomical_twilight_begin
									 :sun/astronomical_twilight_end]))

(s/def :sun/sunrise-spec (s/keys :req-un [:sun/results :sun/status]))
