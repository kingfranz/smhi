(ns smhi.spec
    (:require [smhi.utils        :refer :all])
    (:require [clojure.spec               :as s])
    (:require [clj-time.core              :as t])
    (:require [clj-time.format            :as f])
    (:require [clj-time.local             :as l]))

(s/def ::approvedTime  f/parse)
(s/def ::referenceTime f/parse)
(s/def ::type          (s/and #(string? %) #(= % "Point")))
(s/def ::coordinates   (s/and #(= (count %) 1)
                        #(= (count (first %)) 2)
                        #(-> % first first double?)
                        #(-> % first second double?)))
(s/def ::geometry      (s/keys :req-un [::type ::coordinates]))
(s/def ::validTime     f/parse)
(s/def :hl/levelType   (s/and string? #(= % "hl")))
(s/def :hmsl/levelType (s/and string? #(= % "hmsl")))
(s/def :dec/values     (s/and #(= (count %) 1) #(-> % first number?)))
(s/def :int/values     (s/and #(= (count %) 1) #(-> % first int?)))
(s/def :octas/values   (s/and #(= (count %) 1) #(-> % first int?) #(>= (first %) 0) #(<= (first %) 8)))
(s/def :press/values   (s/and #(= (count %) 1) #(-> % first number?) #(>= (first %) 500) #(<= (first %) 1500)))
(s/def :temp/values    (s/and #(= (count %) 1) #(-> % first number?) #(>= (first %) -60) #(<= (first %) 60)))
(s/def :vis/values     (s/and #(= (count %) 1) #(-> % first number?) #(>= (first %) 0) #(<= (first %) 1000)))
(s/def :degree/values  (s/and #(= (count %) 1) #(-> % first int?) #(>= (first %) 0) #(<= (first %) 360)))
(s/def :percentd/values (s/and #(= (count %) 1) #(-> % first number?) #(>= (first %) 0) #(<= (first %) 102)))
(s/def :percenti/values (s/and #(= (count %) 1) #(-> % first int?) #(>= (first %) 0) #(<= (first %) 102)))
(s/def :percento/values (s/or :per :percenti/values :nine (s/and #(= (count %) 1) #(-> % first int?) #(= (first %) -9))))
(s/def :precat/values   (s/and #(= (count %) 1) #(-> % first int?) #(>= (first %) 0) #(<= (first %) 6)))
(s/def :symb/values     (s/and #(= (count %) 1) #(-> % first int?) #(>= (first %) 1) #(<= (first %) 15)))

(s/def :lvl-zero/level (s/and int? #(= % 0)))
(s/def :lvl-two/level  (s/and int? #(= % 2)))
(s/def :lvl-ten/level  (s/and int? #(= % 10)))

(s/def :msl/name (s/and #(string? %) #(= % "msl")))
(s/def :msl/unit (s/and #(string? %) #(= % "hPa")))
(s/def :msl/params (s/keys :req-un [:msl/name :hmsl/levelType :lvl-zero/level :msl/unit :press/values]))

(defmacro nup
  [pns punit plvl ptype]
  `(do
    (s/def ~(keyword (str pns) "name")   (s/and #(string? %) #(= % (str '~pns))))
    (s/def ~(keyword (str pns) "unit")   (s/and #(string? %) #(= % ~punit)))
    (s/def ~(keyword (str pns) "params") (s/keys :req-un [~(keyword (str pns) "name")
                                                          :hl/levelType
                                                          ~(keyword (str plvl) "level")
                                                          ~(keyword (str pns) "unit")
                                                          ~(keyword (str ptype) "values")]))))

(nup t        "Cel"      lvl-two  temp)
(nup vis      "km"       lvl-two  vis)
(nup wd       "degree"   lvl-ten  degree)
(nup ws       "m/s"      lvl-ten  percentd)
(nup r        "percent"  lvl-two  percenti)
(nup tstm     "percent"  lvl-zero percenti)
(nup tcc_mean "octas"    lvl-zero octas)
(nup lcc_mean "octas"    lvl-zero octas)
(nup mcc_mean "octas"    lvl-zero octas)
(nup hcc_mean "octas"    lvl-zero octas)
(nup gust     "m/s"      lvl-ten  percentd)
(nup pmin     "kg/m2/h"  lvl-zero percentd)
(nup pmax     "kg/m2/h"  lvl-zero percentd)
(nup spp      "percent"  lvl-zero percento)
(nup pcat     "category" lvl-zero precat)
(nup pmean    "kg/m2/h"  lvl-zero percentd)
(nup pmedian  "kg/m2/h"  lvl-zero percentd)
(nup Wsymb    "category" lvl-zero symb)

(s/def ::parameters (s/cat  :mslp      :msl/params
                     :tp        :t/params
                     :visp      :vis/params
                     :wdp       :wd/params
                     :wsp       :ws/params
                     :rp        :r/params
                     :tstmp     :tstm/params
                     :tcc_meanp :tcc_mean/params
                     :lcc_meanp :lcc_mean/params
                     :mcc_meanp :mcc_mean/params
                     :hcc_meanp :hcc_mean/params
                     :gustp     :gust/params
                     :pminp     :pmin/params
                     :pmaxp     :pmax/params
                     :sppp      :spp/params
                     :pcatp     :pcat/params
                     :pmeanp    :pmean/params
                     :pmedianp  :pmedian/params
                     :Wsymbp    :Wsymb/params))
(s/def ::timeSeries (s/+ (s/keys :req-un [::validTime ::parameters])))
(def smhi-spec (s/keys :req-un [::approvedTime ::referenceTime ::geometry ::timeSeries]))

;;---------------------------------------------------------------------------------

(def sunrise-example
	{
		:results
		{
			:sunrise "2015-05-21T05:05:35+00:00"
			:sunset "2015-05-21T19:22:59+00:00"
			:solar_noon "2015-05-21T12:14:17+00:00"
			:day_length 51444
			:civil_twilight_begin "2015-05-21T04:36:17+00:00"
			:civil_twilight_end "2015-05-21T19:52:17+00:00"
			:nautical_twilight_begin "2015-05-21T04:00:13+00:00"
			:nautical_twilight_end "2015-05-21T20:28:21+00:00"
			:astronomical_twilight_begin "2015-05-21T03:20:49+00:00"
			:astronomical_twilight_end "2015-05-21T21:07:45+00:00"
		}
		:status "OK"
	})

(s/def :sun/status                      #(= % "OK"))
(s/def :sun/sunrise                     f/parse)
(s/def :sun/sunset                      f/parse)
(s/def :sun/solar_noon                  f/parse)
(s/def :sun/day_length                  is-pos-int?)
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

(def sunrise-spec (s/keys :req-un [:sun/results :sun/status]))
