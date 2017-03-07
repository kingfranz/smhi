(ns smhi.utils
    (:require [smhi.config                :as conf]
              [clojure.data.json          :as json]
              [clojure.java.io            :as io]
              [clojure.spec               :as s]
              [clojure.string             :as str]
              [clj-time.core              :as t]
              [clj-time.format            :as f]
              [clj-time.local             :as l]
              [clojure.math.numeric-tower :as math]
              [seesaw.timer               :as st]
              [seesaw.core                :as sc]
              [seesaw.border              :as sb]
              [seesaw.graphics            :as sg]
              [seesaw.color               :as sclr]
              [seesaw.font                :as sf]
              [org.httpkit.client         :as http]
              [taoensso.timbre            :as timbre]
              [taoensso.timbre.appenders.core :as appenders])
    (:import (javax.swing JFrame JLabel)
             (java.awt Color Font FontMetrics GraphicsEnvironment)
             (java.io ByteArrayInputStream)))

; return current dat & time as a string
(defn now-str
    []
    (f/unparse (f/with-zone (f/formatters :mysql) (t/default-time-zone)) (l/local-now)))

(defn send-request
    [url resp-type]
    (let [ret-type (if (= resp-type :json) :text :byte-array)
          {:keys [status headers body error] :as resp} @(http/get url {:timeout conf/smhi-timeout :as ret-type})]
      (if error
          (if (instance? org.httpkit.client.TimeoutException error)
            (throw (Exception. "Timeout"))
            (throw (Exception. "unknown network errror")))
          body)))

(defn send-json-request
	[url]
	(let [response (send-request url :json)]
		(try
        	(json/read-str (clojure.string/join response) :key-fn keyword)
		    (catch Exception je
		    	(spit "clock-error.log" (str (now-str) "\n" response) :append true)
		    	(throw je)))))

(defn read-image
    [fname]
    (javax.imageio.ImageIO/read (java.io.File.
        (str (when-not (clojure.string/includes? fname "/") (:image-dir @conf/config)) fname))))

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
    (get-dir-list (:image-dir @conf/config) #"background-\d+\.(png|jpg|jpeg)$"))

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
    (Integer/parseInt s))

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
  (timbre/info (format "New size: %s width: %d height: %d" k (:width v) (:height v))))

; keep track of window sizes for logging
(defn lbl-info
    [object ^java.awt.Graphics2D g2d]
    (let [id     (sc/id-of object)
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
    (map #(f/unparse (f/formatter "EEE dd/MM") (t/plus (l/local-now) (t/days %))) (range (:graph-days @conf/config))))

(def clock-pics (atom nil))

(def symbol-pics (atom nil))

(def tiny-symbol-pics (atom nil))

(defn setup-images
    []
    (reset! tiny-symbol-pics (into {} (map #(hash-map % (read-image (format "symbol-%02dAs.png" %))) (range 16))))
    (reset! symbol-pics (into {} (map #(hash-map % (read-image (format "symbol-%02dA.png" %))) (range 1 16))))
    (reset! clock-pics {:map-pic   (read-image "map3D.png")
                        :hour-hand (read-image "clock-hour.png")
                        :min-hand (read-image "clock-minute.png")
                        :sec-hand (read-image "clock-second.png")
                        :clock-pic (read-image "clock-rim.png")
                        :compass-pic (read-image "compass.png")
                        :arrow-pic (read-image "arrow.png")}))

(defn rotate-file
    [log path prefix num-files]
    (let [dir     (get-dir-list path (re-pattern (str prefix "-\\d+\\.log")))
          extract (fn [x] (str/replace x #"^([^-]+-)([0-9]+)(\.log)$" "$2"))
          numbers (map extract dir)
          biggest (->> numbers (map parse-int) sort last)
          new-big (str path prefix "-" (if (nil? biggest) 0 (inc biggest)) ".log")]
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
