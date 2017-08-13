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
                (image-resizer  [resize   :refer :all]
                                [core     :refer :all]
                                [scale-methods :refer :all])
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
    (javax.imageio.ImageIO/write image "png" (java.io.File. fname))
    image)

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

(defn longitude->x
  	[l]
   	(+ (* l (config :master-map-kx)) (config :master-map-mx)))

(defn x->longitude
  	[x]
   	(/ (- x (config :master-map-mx)) (config :master-map-kx)))

(defn latitude->y
  	[l]
   	(+ (* l (config :master-map-ky)) (config :master-map-my)))

(defn y->latitude
  	[y]
   	(/ (- y (config :master-map-my)) (config :master-map-ky)))

(defn mk-map
  	[]
   	(let [fname (str (config :image-dir)
                     "map-" (config :longitude) "-" (config :latitude)
                     "-" (config :radar-width) "-" (config :radar-height) ".png")]
      	(if (.exists (io/as-file fname))
         	(read-image fname)
          	(let [master    	  (read-image (config :master-map))
                  width-longitude (config :radar-width-long)
                  calc-latitude   (- 90.0 (config :latitude))
                  ulx       	  (longitude->x (- (config :longitude) (/ width-longitude 2)))
                  lrx       	  (longitude->x (+ (config :longitude) (/ width-longitude 2)))
                  sub-width 	  (- lrx ulx)
                  h-w-ratio 	  (/ (config :radar-height) (config :radar-width))
                  sub-height 	  (* sub-width h-w-ratio)
                  center-y    	  (latitude->y  calc-latitude)
                  uly             (- center-y (/ sub-height 2))
                  lry       	  (+ center-y (/ sub-height 2))]
             	(if (and (>= ulx 0) (>= uly 0) (> lrx ulx) (> lry uly)
                 		   (< lrx (.getWidth master)) (< lry (.getHeight master)))
                	(as-> master $
                  		  (.getSubimage $ ulx uly sub-width sub-height)
          		  		  (resize-to-width $ (config :radar-width))
      			  		  (write-image fname $))
                 	(sg/buffered-image (config :radar-width) (config :radar-height)))))))

(defn setup-images
    []
    (log/info "enter: setup-images")
    (reset! tiny-symbol-pics* (into {} (map (fn [i]
                          {i (read-image (format "symbol-%02dAs.png" i))})
                          (range 16))))
    (reset! symbol-pics* (into {} (map (fn [i]
                          {i (read-image (format "symbol-%02dA.png" i))})
                          (range 1 16))))
    (reset! clock-pics* {
        :map-pic     (mk-map)
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

