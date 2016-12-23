(ns smhi.config
    (:use seesaw.core)
    (:use seesaw.border)
    (:use seesaw.graphics)
    (:use seesaw.color)
    (:use seesaw.font)
 )


(def smhi-config
    {:latitude    "58.786869"
     :longitude   "14.265020"
     :weather-url "http://opendata-download-metfcst.smhi.se"
     :radar-url   "http://opendata-download-radar.smhi.se/api/version/latest/area/sweden/product/comp/latest.png"
     :category    "pmp2g"
     :version     "2"})

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

(def image-dir "resources/")

(def res-scale       1)
(def horiz-res       (* 1920 res-scale))
(def vert-rez        (* 1080 res-scale))
(def graphics-height (* vert-rez 1/3))
(def graphics-width  horiz-res)
(def clock-height    (- vert-rez graphics-height))
(def clock-width     clock-height)
(def radar-height    (* clock-height 2/3))
(def radar-width     (- horiz-res clock-width))
(def info-height     (- clock-height radar-height))
(def info-width      radar-width)
(def wnow-height     (/ (- clock-height radar-height) 2))
(def wnow-width      (/ radar-width 5))

(def left-axis-width  100)
(def right-axis-width 50)
(def max-rain-level   5)
(def graph-days       7)
(def week-minutes     (* graph-days 24 60))
(def temp-padding     5)
(def wind-padding     2)
(def tick-width       10)
(def temp-text-x      (- left-axis-width tick-width 3))
(def wind-text-x      (- (/ left-axis-width 2) tick-width 3))
(def tot-temp-span    20)
(def axis-span        10)
(def rain-axis-span   max-rain-level)
(def wind-axis-factor 3)
(def smhi-timeout     5000)

(def radar-sub-width        112)
(def radar-sub-height       45)
(def radar-sub-upper-left-x 110)
(def radar-sub-upper-left-y 595)

(def axis-width     2)
(def wind-style     (style :foreground :white :background :lightgray))
(def rain-style     (style :foreground :blue  :background :blue))
(def temp-style     (style :foreground :red   :stroke 3))
(def axis-style     (style :foreground :white :stroke axis-width))
(def day-tick-style (style :foreground :white :stroke 1))

(def axis-font-name       (str "ARIAL-" (int (* res-scale 20))))
(def axis-txt-font        (font axis-font-name))
(def temp-axis-text-style (style :foreground :red :font axis-font-name))
(def wind-axis-text-style (style :foreground :grey :font axis-font-name))
(def rain-axis-text-style (style :foreground :blue :font axis-font-name))
(def zero-line-style      (style :foreground :white :stroke 1))
(def lbl-txt-color        "#FFFFFF")
(def lbl-txt-font         (str "ARIAL-" (int (* res-scale 48))))
(def lbl-info-txt-font    (str "ARIAL-" (int (* res-scale 18))))
(def degree-char          "\u00b0")
(def text-circle-style    (style :foreground :white :background :white))
(def info-title-style     (style :foreground :white :font lbl-info-txt-font))
(def info-value-style     (style :foreground :white :font lbl-txt-font))
(def info-bg-style        (style :foreground (color 32 32 32)
                           :stroke 2
                           :background (color 128 128 128 128)))
(def date-txt-style       (style :foreground :white :font lbl-info-txt-font))
(def exception-style      (style :foreground :red :font "ARIAL-64"))
