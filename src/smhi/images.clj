(ns smhi.images
    (:require 	(smhi 			[config   :refer [config]]
                     			[utils    :refer :all]
                        		[sun      :as sun])
              	(clojure.data 	[json     :as json])
              	(clojure.java 	[io       :as io])
              	(clojure.spec 	[alpha    :as s])
              	(clojure 		[string   :as str])
              	(clj-time 		[core     :as t]
              					[format   :as f]
              					[local    :as l])
              	(seesaw 		[timer    :as st]
              					[core     :as sc]
              					[border   :as sb]
              					[graphics :as sg]
              					[color    :as sclr]
              					[font     :as sf])
              	(org.httpkit 	[client   :as http])
              	(taoensso 		[timbre   :as log])))

;;-----------------------------------------------------------------------------

(def ^:private clock-pics* (atom nil))

(def ^:private symbol-pics* (atom nil))

(def ^:private tiny-symbol-pics* (atom nil))

(def landscape-pic (atom nil))

;;-----------------------------------------------------------------------------

(defn read-image
    [fname]
    ;(log/info "enter: read-image")
    (javax.imageio.ImageIO/read (java.io.File.
        (str (when-not (clojure.string/includes? fname "/") (config :image-dir)) fname))))

(defn write-image
    [fname image]
    ;(log/info "enter: read-image")
    (javax.imageio.ImageIO/write image "png" (java.io.File. fname)))

(defn get-background-list
  	"get list of available background images"
    []
    (log/info "enter: get-background-list")
    (get-dir-list (config :image-dir) #"background-\d+\.(png|jpg|jpeg)$"))

(defn get-background-name
  	"pick a random background"
    []
    (log/info "enter: get-background-name")
    (let [backgrounds (get-background-list)
          num-bg      (count backgrounds)]
        (nth backgrounds (rand-int num-bg))))

(defn setup-images
    []
    (log/info "enter: setup-images")
    (reset! tiny-symbol-pics* (into {} (map (fn [i]
                          {i (read-image (format "symbol-%02dAs.png" i))})
                          (range 16))))
    (reset! symbol-pics* (into {} (map (fn [i]
                          {i (read-image (format "symbol-%02dA.png" i))})
                          (range 1 16))))
    (reset! clock-pics* {:map-pic     (read-image "map3D.png")
                        :hour-hand   (read-image "clock-hour.png")
                        :min-hand    (read-image "clock-minute.png")
                        :sec-hand    (read-image "clock-second.png")
                        :clock-pic   (read-image "clock-rim.png")
                        :compass-pic (read-image "compass.png")
                        :arrow-pic   (read-image "arrow.png")}))

(defn clock-pics
  	[k]
   	{:pre [(q-valid? keyword? k)
           (q-valid? some? @clock-pics*)
           (q-valid? map? @clock-pics*)
           (q-valid? some? (get @clock-pics* k))]}
   	;(log/info "enter: clock-pics")
    (get @clock-pics* k))

(defn symbol-pics
  	[k]
   	{:pre [(q-valid? (s/int-in 1 16) k)
           (q-valid? some? @symbol-pics*)
           (q-valid? map? @symbol-pics*)
           (q-valid? some? (get @symbol-pics* k))]}
   	;(log/info "enter: symbol-pics")
    (get @symbol-pics* k))

(defn tiny-symbol-pics
  	[k]
   	{:pre [(q-valid? (s/int-in 0 16) k)
           (q-valid? some? @tiny-symbol-pics*)
           (q-valid? map? @tiny-symbol-pics*)
           (q-valid? some? (get @tiny-symbol-pics* k))]}
   	;(log/info "enter: tiny-symbol-pics")
    (get @tiny-symbol-pics* k))

(defn get-background
  	"load a background image"
    []
    ;(log/info "enter: set-background")
    (->> (get-background-name)
         (read-image)))

(defn set-background
  	"load an image and set it as background"
    [image]
    ;(log/info "enter: set-background")
    (reset! landscape-pic image))

;;-----------------------------------------------------------------------------

