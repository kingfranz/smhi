(ns smhi.config
   (:require [clojure.xml          :as cxml]
             [clojure.data.xml     :as xml]
             [clojure.data.zip.xml :as zip-xml]
             [clojure.zip          :as zip]
             [clojure.java.io      :as io]
             [seesaw.core          :as sc]
             [seesaw.border        :as sb]
             [seesaw.graphics      :as sg]
             [seesaw.color         :as sclr]
             [seesaw.font          :as sf]))

(def units
  {:msl      {:str "Air Pressure"            :is-int false :keep true  :unit "hPa"}
   :t        {:str "Temperature"             :is-int false :keep true  :unit "C"}
   :vis      {:str "Visibility"              :is-int false :keep false :unit "km"}
   :wd       {:str "Wind Dir"                :is-int true  :keep true  :unit "degree"}
   :ws       {:str "Wind Speed"              :is-int false :keep true  :unit "m/s"}
   :r        {:str "Humidity"                :is-int true  :keep true  :unit "%"}
   :tstm     {:str "Thunder"                 :is-int true  :keep true  :unit "%"}
   :tcc_mean {:str "Total Clouds"            :is-int true  :keep true  :unit "octas"}
   :lcc_mean {:str "Low Clouds"              :is-int true  :keep false :unit "octas"}
   :mcc_mean {:str "Medium Clouds"           :is-int true  :keep false :unit "octas"}
   :hcc_mean {:str "High Clouds"             :is-int true  :keep false :unit "octas"}
   :gust     {:str "Gust Speed"              :is-int false :keep true  :unit "m/s"}
   :pmin     {:str "Min Precip"              :is-int false :keep true  :unit "mm/h"}
   :pmax     {:str "Max Precip"              :is-int false :keep true  :unit "mm/h"}
   :spp      {:str "Percent Frozen"          :is-int true  :keep true  :unit "%"}
   :pcat     {:str "Precip Category"         :is-int true  :keep true  :unit "category"}
   :pmean    {:str "Mean Precip Intensity"   :is-int false :keep true  :unit "mm/h"}
   :pmedian  {:str "Median Precip Intensity" :is-int false :keep true  :unit "mm/h"}
   :Wsymb    {:str "Symbol"                  :is-int true  :keep true  :unit "code"}})

(def res-scale             1)
(def axis-font-name        (str "ARIAL-" (int (* res-scale 20))))
(def axis-width            2)

(def config (atom nil))

(def horiz-res             (* 1920 res-scale))
(def vert-rez              (* 1080 res-scale))
(def graphics-height       (* vert-rez 1/3))
(def graphics-width        horiz-res)
(def clock-height          (- vert-rez graphics-height))
(def clock-width           clock-height)
(def radar-height          (* clock-height 2/3))
(def radar-width           (- horiz-res clock-width))
(def info-height           (- clock-height radar-height))
(def info-width            radar-width)
(def wnow-height           (/ (- clock-height radar-height) 2))
(def wnow-width            (/ radar-width 5))
(def sun-point             {:x (+ radar-width (/ clock-width 2)) :y (- (/ clock-height 2) 100)})
(def date-point            {:x (+ radar-width (/ clock-width 2)) :y (+ (/ clock-height 2) 150)})

(def left-axis-width       100)
(def right-axis-width      50)
(defn week-minutes []         (* (:graph-days @config) 24 60))
(def temp-padding          5)
(def wind-padding          2)
(def tick-width            10)
(def temp-text-x           (- left-axis-width tick-width 3))
(def wind-text-x           (- (/ left-axis-width 2) tick-width 3))
(def axis-span             10)
(defn rain-axis-span []       (:max-rain-level @config))
(def wind-axis-factor      3)
(def smhi-timeout          5000)
(def twilight-begin        :civil_twilight_begin)
(def twilight-end          :civil_twilight_end)

(defn radar-interval-ms []    (* (:radar-interval-minutes @config) 60 1000))
(defn max-radar-queue-size [] (* (/ 60 (:radar-interval-minutes @config)) (:radar-ani-hours @config)))
(def lbl-txt-color         "#FFFFFF")
(def lbl-txt-font          (str "ARIAL-" (int (* res-scale 48))))
(def lbl-info-txt-font     (str "ARIAL-" (int (* res-scale 18))))
(def degree-char           "\u00b0")
(def info-title-style      (sg/style :foreground :white :font lbl-info-txt-font))
(def info-value-style      (sg/style :foreground :white :font lbl-txt-font))
(def info-bg-style         (sg/style :foreground (sclr/color 32 32 32) :stroke 2
						   		       :background (sclr/color 128 128 128 128)))
(def date-txt-style        (sg/style :foreground :white :font lbl-info-txt-font))
(def exception-style       (sg/style :foreground :red :font "ARIAL-64"))
(def sun-style             (sg/style :foreground :black :font "ARIAL-BOLD-36"))
(def sun-bg-style          (sg/style :foreground :black :stroke 2 :background (sclr/color 140 140 140)))
(def radar-txt-style       (sg/style :foreground :black :font "ARIAL-BOLD-64"))

(def fixed-temp            true)
(defn tot-temp-span []        (if fixed-temp (- (:max-fixed-temp @config) (:min-fixed-temp @config)) 30))

(defn parse-int
   [s]
   (Integer/parseInt s))

(defn zip-txt
   [elems default-value]
   (or (some->> elems (apply zip-xml/xml1->) zip-xml/text) default-value))

(defn zip-int
   [elems default-value]
   (or (some->> (zip-txt elems nil) parse-int) default-value))

(defn zip-kw
   [elems default-value]
   (or (some->> (zip-txt elems nil) keyword) default-value))

(defn xml->style
   [default-fg default-bg default-stroke elems]
   ;(println "xml->style:" (zip-kw  (conj elems :foreground)))
   (sg/style :foreground (zip-kw  (conj elems :foreground) default-fg)
             :background (zip-kw  (conj elems :background) default-bg)
             :stroke     (zip-int (conj elems :stroke) default-stroke)))

(defn xml->fontstyle
   [default-fg default-bg default-stroke _font elems]
   (sg/update-style (xml->style default-fg default-bg default-stroke elems) :font _font))

(defn xml->config
   [root]
   (let [axis-txt-font* (sf/font (zip-txt [root :axis :font] (str "ARIAL-" (int (* res-scale 20)))))]
   {
   :max-rain-level         (zip-int [root :graph :max-rain-level] 5)
   :graph-days             (zip-int [root :graph :graph-days] 7)
   :radar-sub-width        (zip-int [root :radar-image :radar-sub-width] 112)
   :radar-sub-height       (zip-int [root :radar-image :radar-sub-height] 45)
   :radar-sub-upper-left-x (zip-int [root :radar-image :radar-sub-upper-left-x] 110)
   :radar-sub-upper-left-y (zip-int [root :radar-image :radar-sub-upper-left-y] 595)
   :radar-interval-minutes (zip-int [root :radar-image :radar-interval-minutes] 5)
   :radar-fps              (zip-int [root :radar-image :radar-fps] 10)
   :radar-ani-hours        (zip-int [root :radar-image :radar-ani-hours] 5)
   :radar-ani-delay-sec    (zip-int [root :radar-image :radar-ani-delay-sec] 2)
   :wind-style             (xml->style :white :lightgray 1 [root :graph :wind-style])
   :rain-style             (xml->style :blue :blue 1 [root :graph :rain-style])
   :temp-style             (xml->style :red :red 3 [root :graph :temp-style])
   :axis-style             (xml->style :white :white axis-width [root :axis :axis-style])
   :day-tick-style         (xml->style :white :white 1 [root :axis :day-tick-style])
   :axis-txt-font          axis-txt-font*
   :temp-axis-text-style   (xml->fontstyle :red :red 1 axis-txt-font* [root :axis :temp-axis-txt-style])
   :wind-axis-text-style   (xml->fontstyle :grey :grey 1 axis-txt-font* [root :axis :wind-axis-txt-style])
   :rain-axis-text-style   (xml->fontstyle :blue :blue 1 axis-txt-font* [root :axis :rain-axis-txt-style])
   :zero-line-style        (xml->style :white :white 1 [root :axis :zero-line-style])
   :text-circle-style      (xml->style :white :white 1 [root :axis :text-circle-style])
   :min-fixed-temp         (zip-int [root :graph :temp-range :min] -20)
   :max-fixed-temp         (zip-int [root :graph :temp-range :max] 30)
   :image-dir              (zip-txt [root :image-dir] "resources/")
   :latitude               (zip-txt [root :location :latitude] "58.786869")
   :longitude              (zip-txt [root :location :longitude] "14.265020")
   :weather-url            (zip-txt [root :smhi :weather-url] "http://opendata-download-metfcst.smhi.se")
   :radar-url              (zip-txt [root :smhi :radar-url] "http://opendata-download-radar.smhi.se/api/version/latest/area/sweden/product/comp/latest.png")
   :category               (zip-txt [root :smhi :category] "pmp2g")
   :version                (zip-txt [root :smhi :version] "2")
   }))

(defn read-config-file
   []
   (let [root (zip/xml-zip (xml/parse-str (slurp "smhi-config.xml")))
         new-config (xml->config root)]
      (reset! config new-config)))

