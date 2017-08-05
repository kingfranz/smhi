(ns smhi.utils
    (:require 	(smhi 			[config   :refer [config]])
              	(clojure.data 	[json     :as json])
              	(clojure.java 	[io       :as io])
              	(clojure 		[string   :as str])
               	(clojure.spec 	[alpha    :as s])
              	(clj-time 		[core     :as t]
              					[format   :as f]
              					[local    :as l])
              	(org.httpkit 	[client   :as http])
              	(taoensso 		[timbre   :as log]))
    (:import (javax.swing 	JFrame JLabel)
             (java.awt 		Color Font FontMetrics GraphicsEnvironment)
             (java.awt.font TextLayout)
             (java.io 		ByteArrayInputStream File)
             (javax.imageio ImageIO))
    )

;;-----------------------------------------------------------------------------

(defmacro q-valid?
	[sp v]
	`(if-not (s/valid? ~sp ~v)
		(log/error
			(str "\n---------- " ~*file* " " ~(:line (meta &form)) " ------------\n"
				 ~v
				 "\n---------------------------------------\n"
				 (s/explain-str ~sp ~v)
				 "\n---------------------------------------\n"))
		true))

(defn spy
	([x]
	(log/debug "spy:" (type x) ">>" (if (coll? x) (doall (for [e x] (pr-str e " "))) x) "<<")
	x)
	([t x]
	(log/debug "spy:" t (type x) ">>" (if (coll? x) (doall (for [e x] (pr-str e " "))) x) "<<")
	x))

(defn qspy
  	[txt coll]
    (log/debug txt " " (doall (for [e coll] (str e " "))))
    coll)

(defn current-year
	[]
	(t/year (l/local-now)))

(defn current-month
	[]
	(t/month (l/local-now)))

(defn hour-minute
  	([]
   	(hour-minute (l/local-now)))
  	([dt]
   	(f/unparse (f/with-zone (f/formatters :hour-minute) (t/default-time-zone)) dt)))

;;-----------------------------------------------------------------------------

(defn now-str
  	"return current dat & time as a string"
    []
    (f/unparse (f/with-zone (f/formatters :mysql) (t/default-time-zone)) (l/local-now)))

(defn send-request
    [url resp-type]
    (let [ret-type (if (= resp-type :json) :text :byte-array)
          {:keys [status headers body error] :as resp} @(http/get url {:timeout (config :smhi-timeout) :as ret-type})]
      	(if error
        	(if (instance? org.httpkit.client.TimeoutException error)
          		(throw (Exception. "Timeout"))
            	(throw (Exception. "unknown network errror")))
          	body)))

(defn fix-text
	[r func]
	(let [r* (if (coll? r) (str/join r) r)]
		(if (some? func)
			(func r*)
			r*)))

(defn send-json-request
	([url]
	(send-json-request url nil))
	([url func]
	(let [response (send-request url :json)]
		(try
        	(-> response
        		(fix-text func)
        		(json/read-str :key-fn keyword))
		    (catch Exception je
		    	(spit "clock-error.log" (str (now-str) "\n" response) :append true)
		    	(throw je))))))

(defn abs
    [x]
    (if (< x 0)
      (- 0 x)
      x))

(defn half
    [x]
    (/ x 2))

(defn neg
  	[x]
    (- 0 x))

(defn get-dir-list
  	"read a directory"
    [dir re]
    (filter #(re-find re %) (map str (file-seq (io/file dir)))))

(defn byte-array-2-image
    [barray]
    (javax.imageio.ImageIO/read (ByteArrayInputStream. barray)))

(defn string-width
  	"calculate width of string (in pixels)"
  	[^java.awt.Graphics2D g2d txt-style txt]
    (if (and (some? g2d) (some? txt-style) (some? txt) (some? (:font txt-style)))
  		(.stringWidth (.getFontMetrics g2d (:font txt-style)) txt)
    	(do
       		(prn "string-width:" g2d txt-style txt)
         	200)))

(defn string-height
  	"calculate height of string (in pixels)"
    [^java.awt.Graphics2D g2d txt-style]
    (if (and (some? g2d) (some? txt-style) (some? (:font txt-style)))
  		(.getHeight (.getFontMetrics g2d (:font txt-style)))
		(do
       		(prn "string-height" g2d txt-style)
         	200)))

(defn get-screens
    []
    (let [ge     (GraphicsEnvironment/getLocalGraphicsEnvironment)
          sd     (.getScreenDevices ge)
          bounds (fn [x] (.getBounds (.getDefaultConfiguration x)))]
        (map #(hash-map :x      (.x (bounds %))
                        :y      (.y (bounds %))
                        :width  (.width (bounds %))
                        :height (.height (bounds %)))
             sd)))

(defn is-pos-int-str?
    [s]
    (and (string? s) (re-matches #"\d+" (str/trim s))))

(defn is-pos-int?
    [s]
    (and (int? s) (pos? s)))

(defn hour-to-angle
  	"convert hour to 0-360"
    [h]
    {:pre [(and (>= h 0) (< h 24))]}
    (* (/ (mod h 12) 12) 360))

(defn wind-dir-to-str
  	"map wind direction angle to text"
    [dir]
    {:pre  [(and (>= dir 0) (<= dir 360))]
     :post [(string? %)]}
    (let [between (fn [x [l h]] (and (>= x l) (< x h)))
          wd [[[  0.0  22.5] "N"]
              [[ 22.5  67.5] "NE"]
              [[ 67.5 112.5] "E"]
              [[112.5 157.5] "SE"]
              [[157.5 202.5] "S"]
              [[202.5 247.5] "SW"]
              [[247.5 292.5] "W"]
              [[292.5 337.5] "NW"]
              [[337.5 360.1] "N"]]]
        (->> wd (filter #(between dir (first %))) first second)))

(defn avg
    [coll]
    (if (empty? coll)
        0
        (int (/ (apply + coll) (count coll)))))

(defn mk-date-strings
    []
    (map #(f/unparse (f/formatter "EEE dd/MM") (t/plus (l/local-now) (t/days %)))
    	 (range (config :graph-days))))

(defn day-of-week
	[]
	(dec (t/day-of-week (l/local-now))))

