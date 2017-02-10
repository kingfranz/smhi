(ns smhi.utils
    (:require [smhi.config        :refer :all])
    (:require [clojure.data.json          :as json])
    (:require [clojure.java.io            :as io])
    (:require [clojure.spec               :as s])
    (:require [clojure.string             :as str])
    (:require [clj-time.core              :as t])
    (:require [clj-time.format            :as f])
    (:require [clj-time.local             :as l])
    (:require [clojure.math.numeric-tower :as math])
    (:require [seesaw.timer               :as st])
    (:require [org.httpkit.client         :as http])
    (:require [taoensso.timbre            :as timbre
               :refer [log  trace  debug  info  warn  error  fatal  report
                       logf tracef debugf infof warnf errorf fatalf reportf spy get-env]])
    (:require [taoensso.timbre.appenders.core :as appenders])
    (:use seesaw.core)
    (:use seesaw.border)
    (:use seesaw.graphics)
    (:use seesaw.color)
    (:use seesaw.font)
    (:import (javax.swing JFrame JLabel)
             (java.awt Color Font FontMetrics GraphicsEnvironment)
             (java.io ByteArrayInputStream)))

(defn set-var 
	"set value of atom"
	[the-atom value]
	(swap! the-atom (fn [x] value)))

(defn send-request
    [url resp-type]
    (let [ret-type (if (= resp-type :json) :text :byte-array)
          {:keys [status headers body error] :as resp} @(http/get url {:timeout smhi-timeout :as ret-type})]
      (if error
          (if (instance? org.httpkit.client.TimeoutException error)
            (throw (Exception. "Timeout"))
            (throw (Exception. "unknown network errror")))
          body)))

(defn send-json-request
	[url]
    (-> url (send-request :json)
        	(clojure.string/join)
        	(json/read-str :key-fn keyword)))

(defn read-image
    [fname]
    (javax.imageio.ImageIO/read (java.io.File. (str (if-not (clojure.string/includes? fname "/") image-dir) fname))))

; return current dat & time as a string
(defn now-str
    []
    (f/unparse (f/with-zone (f/formatters :mysql) (t/default-time-zone)) (l/local-now)))

(defn abs
    [x]
    (if (< x 0)
      (- 0 x)
      x))

; read a directory
(defn get-dir-list
    [dir re]
    (filter #(re-find re %) (map str (file-seq (io/file dir)))))

; get list of available background images
(defn get-background-list
    []
    (get-dir-list image-dir #"background-\d+\.(png|jpg|jpeg)$"))

; pick a random background
(defn get-background-name
    []
    (let [backgrounds (get-background-list)
          num-bg      (count backgrounds)]
        (nth backgrounds (rand-int num-bg))))

(defn byte-array-2-image
    [barray]
    (javax.imageio.ImageIO/read (ByteArrayInputStream. barray)))

; calculate width of string (in pixels)
(defn string-width
  [^java.awt.Graphics2D g2d txt-style txt]
  (.stringWidth (.getFontMetrics g2d (:font txt-style)) txt))

; calculate height of string (in pixels)
(defn string-height
    [^java.awt.Graphics2D g2d txt-style]
    (.getHeight (.getFontMetrics g2d (:font txt-style))))

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

(defn not-nil?
    [params]
    (not (nil? params)))

(defn parse-int [s]
    (Integer. (re-find  #"\d+" (str/trim s))))

(defn is-string?
    [s]
    (and (not-nil? s) (string? s) (> (count (str/trim s)) 0)))

(defn is-pos-int-str?
    [s]
    (and (is-string? s) (re-matches #"\d+" (str/trim s))))

(defn is-pos-int?
    [s]
    (and (int? s) (pos? s)))

; the window size map
(def lbl-map (atom nil))

  ; print changed window sizes for logging
(defn update-lbl-map
  [m k v]
  (swap! lbl-map (fn [x] (assoc m k v)))
  (info (format "New size: %s width: %d height: %d" k (:width v) (:height v))))

; keep track of window sizes for logging
(defn lbl-info
    [object ^java.awt.Graphics2D g2d]
    (let [id     (id-of object)
          width  (.getWidth object)
          height (.getHeight object)]
        (if (nil? @lbl-map)
            (update-lbl-map {} id {:width width :height height})
            (if (contains? @lbl-map id)
                (let [w (-> @lbl-map (get id) :width)
                      h (-> @lbl-map (get id) :height)]
                    (if (not (and (= w width) (= h height)))
                        (update-lbl-map @lbl-map id {:width width :height height})))
                (update-lbl-map @lbl-map id {:width width :height height})))))

; convert hour to 0-360
(defn hour-to-angle
    [h]
    {:pre [(and (>= h 0) (< h 24))]}
    (* (/ (mod h 12) 12) 360))

; map wind direction angle to text
(defn wind-dir-to-str
    [dir]
    {:pre  [(and (>= dir 0) (<= dir 360))]
     :post [(is-string? %)]}
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
    (map #(f/unparse (f/formatter "EEE dd/MM") (t/plus (l/local-now) (t/days %))) (range graph-days)))

(def map-pic       (read-image "map3D.png"))
(def hour-hand     (read-image "clock-hour.png"))
(def min-hand      (read-image "clock-minute.png"))
(def sec-hand      (read-image "clock-second.png"))
(def clock-pic     (read-image "clock-rim.png"))
(def compass-pic   (read-image "compass.png"))
(def arrow-pic     (read-image "arrow.png"))

(def symbol-pics   { 1 (read-image "symbol-01A.png")
                    2 (read-image "symbol-02A.png")
                    3 (read-image "symbol-03A.png")
                    4 (read-image "symbol-04A.png")
                    5 (read-image "symbol-05A.png")
                    6 (read-image "symbol-06A.png")
                    7 (read-image "symbol-07A.png")
                    8 (read-image "symbol-08A.png")
                    9 (read-image "symbol-09A.png")
                    10 (read-image "symbol-10A.png")
                    11 (read-image "symbol-11A.png")
                    12 (read-image "symbol-12A.png")
                    13 (read-image "symbol-13A.png")
                    14 (read-image "symbol-14A.png")
                    15 (read-image "symbol-15A.png")})

(def tiny-symbol-pics { 0 (read-image "symbol-00As.png")
                       1 (read-image "symbol-01As.png")
                       2 (read-image "symbol-02As.png")
                       3 (read-image "symbol-03As.png")
                       4 (read-image "symbol-04As.png")
                       5 (read-image "symbol-05As.png")
                       6 (read-image "symbol-06As.png")
                       7 (read-image "symbol-07As.png")
                       8 (read-image "symbol-08As.png")
                       9 (read-image "symbol-09As.png")
                       10 (read-image "symbol-10As.png")
                       11 (read-image "symbol-11As.png")
                       12 (read-image "symbol-12As.png")
                       13 (read-image "symbol-13As.png")
                       14 (read-image "symbol-14As.png")
                       15 (read-image "symbol-15As.png")})

(defn rotate-file
    [log path prefix num-files]
    (let [dir     (get-dir-list path (re-pattern (str prefix "-\\d+\\.log")))
          ;a1 (println "dir:" dir)
          extract (fn [x] (str/replace x #"^([^-]+-)([0-9]+)(\.log)$" "$2"))
          numbers (map extract dir)
          ;a3 (println "numbers:" numbers)
          biggest (->> numbers (map parse-int) sort last)
          ;a4 (println "biggest:" biggest)
          new-big (str path prefix "-" (if (nil? biggest) 0 (inc biggest)) ".log")]
        ;(println "new-big:" new-big)
        (.renameTo log (io/file new-big))
        (.createNewFile log)))

(defn max-size-appender
    "Returns a Rolling file appender. Opts:
    :path      - logfile path.
    :prefix    - first part of filename.
    :max-size  - max size in bytes.
    :num-files - max number of files."
    [& [{:keys [path prefix max-size num-files]
         :or   {path      "./"
                prefix    "rolling"
                max-size  1000000
                num-files 10}}]]
        {:enabled?   true
         :async?     false
         :min-level  nil
         :rate-limit nil
         :output-fn  :inherit
         :fn
            (fn [data]
                (let [{:keys [instant output_]} data
                      output-str (force output_)
                      filename   (str path prefix ".log")]
                    (when-let [log (io/file filename)]
                        (try
                            (when-not (.exists log)
                                (io/make-parents log))
                            (if (.exists log)
                                (if (> (.length log) max-size)
                                    (do
                                        ;(println "length:" (.length log) "max:" max-size)
                                    (rotate-file log path prefix num-files)))
                                (.createNewFile log))
                            (spit filename (with-out-str (println output-str)) :append true)
                            (catch java.io.IOException _)))))})
