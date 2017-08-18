(ns smhi.images
    (:require 	(smhi 			[config   :refer [config]]
                     			[utils    :refer :all]
                        		[graph-utils :refer :all]
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

(def ^:private pictures (atom nil))

(def ^:private hourhands (atom nil))

(def ^:private minutehands (atom nil))

(def ^:private secondhands (atom nil))

(def ^:private large-symbols (atom nil))

(def ^:private small-symbols (atom nil))

(def ^:private current-background (atom nil))

;;-----------------------------------------------------------------------------

(defn get-background-list
  	"get list of available background images"
    []
    (log/info "enter: get-background-list")
    (get-dir-list (config :background-dir) #".+\.(png|jpg|jpeg)$"))

(defn get-background-name
  	"pick a random background"
    []
    (log/info "enter: get-background-name")
    (let [backgrounds   (get-background-list)
          num-bg        (count backgrounds)
          path-and-name (nth backgrounds (rand-int num-bg))
          jf            (java.io.File. path-and-name)]
        (.getName jf)))

(defn mk-map
  	[]
   	(let [mmap (config :master-map)
          fname (str "map-" (:center-x mmap) "-" (:center-y mmap) "-" (:width mmap) ".png")]
      	(if (cache-image-exists? fname)
         	(read-cache-image fname)
          	(let [master (read-image (:filename mmap))
                  height (/ (:width mmap) (/ (config :radar-width) (config :radar-height)))
                  ulx    (- (:center-x mmap) (/ (:width mmap) 2))
                  uly    (- (:center-y mmap) (/ height 2))]
             	(if (and (>= ulx 0) (>= uly 0)
                 		 (< (+ ulx (:width mmap)) (.getWidth master)) (< (+ uly height) (.getHeight master)))
                	(as-> master $
                  		  (.getSubimage $ ulx uly (:width mmap) height)
          		  		  (resize-to-width $ (config :radar-width))
      			  		  (write-cache-image fname $))
                 	(sg/buffered-image (config :radar-width) (config :radar-height)))))))

(defn load-scaled
  	[w h f-name]
    (let [fname (str f-name "-" w "-" h ".png")]
      	(if (cache-image-exists? fname)
         	(read-cache-image fname)
          	(let [img (scale-image w h (read-image (str f-name ".png")))]
             	(write-cache-image fname img)
              	img))))

(defn setup-images
    []
    (log/info "enter: setup-images")
    
    (reset! small-symbols (into {} (pmap (fn [i]
                          {i (load-scaled (config :small-symbol-sz)
                                          (config :small-symbol-sz)
                                          (format "symbol-%02dAs" i))})
                          (range 16))))
    
    (reset! large-symbols (into {} (pmap (fn [i]
                          {i (load-scaled (config :wnow-width)
                                          (config :wnow-height)
                                          (format "symbol-%02dA" i))})
                          (range 1 16))))

;    (reset! hourhands (into {} (pmap (fn [i]
;                          {i (load-scaled (config :clock-width)
;                                          (config :clock-height)
;                                          (format "hourhand-%d" i))})
;                          (range 60))))
    
;    (reset! minutehands (into {} (pmap (fn [i]
;                          {i (load-scaled (config :clock-width)
;                                          (config :clock-height)
;                                          (format "minutehand-%d" i))})
;                          (range 60))))
    
;    (reset! secondhands (into {} (pmap (fn [i]
;                          {i (load-scaled (config :clock-width)
;                                          (config :clock-height)
;                                          (format "secondhand-%d" i))})
;                          (range 60))))
    
    (reset! pictures {
        :map-pic     (mk-map)
        :clock-pic   (load-scaled (config :clock-width)
                                  (config :clock-height)
                                  "clock-rim")
        :compass-pic (load-scaled (config :wnow-width)
                                  (config :wnow-height)
                                  "compass")
        :arrow-pic   (load-scaled (config :wnow-width)
                                  (config :wnow-height)
                                  "arrow")}))

(defn get-pic
  	[k]
   	{:pre [(q-valid? keyword? k)
           (q-valid? some? @pictures)
           (q-valid? map? @pictures)
           (q-valid? some? (get @pictures k))]}
   	;(log/info "enter: clock-pics")
    (get @pictures k))

(defn get-hourhand
  	[i]
   	{:pre [(q-valid? (s/int-in 0 60) i)]}
    (load-scaled (config :clock-width)
                 (config :clock-height)
                 (format "hourhand-%d" i))
    ;(get @hourhands i)
    )

(defn get-minutehand
  	[i]
   	{:pre [(q-valid? (s/int-in 0 60) i)]}
    (load-scaled (config :clock-width)
                  (config :clock-height)
                  (format "minutehand-%d" i))
    ;(get @minutehands i)
    )

(defn get-secondhand
  	[i]
   	{:pre [(q-valid? (s/int-in 0 60) i)]}
    (load-scaled (config :clock-width)
                  (config :clock-height)
                  (format "secondhand-%d" i))
    ;(get @secondhands i)
    )

(defn get-symbol
  	[k]
   	{:pre [(q-valid? (s/int-in 1 16) k)
           (q-valid? some? @large-symbols)
           (q-valid? map? @large-symbols)
           (q-valid? some? (get @large-symbols k))]}
   	;(log/info "enter: symbol-pics")
    (get @large-symbols k))

(defn get-tiny-symbol
  	[k]
   	{:pre [(q-valid? (s/int-in 0 16) k)
           (q-valid? some? @small-symbols)
           (q-valid? map? @small-symbols)
           (q-valid? some? (get @small-symbols k))]}
   	;(log/info "enter: tiny-symbol-pics")
    (get @small-symbols k))

(defn load-background
  	"load a background image"
    []
    (let [fname      (get-background-name)
          name-parts (re-find #"^(.+)\.(.+)$" fname)
          cname      (str (nth name-parts 1) "-" (config :horiz-res) "-" (config :vert-res) "." (nth name-parts 2))]
      	(if (cache-image-exists? cname)
         	(read-cache-image cname)
         	(let [image (->> fname
                             (str (config :background-dir))
                             (java.io.File.)
                             (javax.imageio.ImageIO/read)
         					 (scale-image (config :horiz-res) (config :vert-res)))]
            	(write-cache-image cname image)
             	image))))

(defn set-background
  	"load an image and set it as background"
    [image]
    ;(log/info "enter: set-background")
    (reset! current-background image))

(defn background
  	[]
   	@current-background)

;;-----------------------------------------------------------------------------

