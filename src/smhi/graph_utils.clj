(ns smhi.graph-utils
    (:require [smhi.utils                 :as utils]
            [smhi.graph-utils           :as gutils]
            [smhi.spec                  :as spec]
            [smhi.config                :as conf]
            [smhi.sun                   :as sun]
            [clojure.data.json          :as json]
            [clojure.java.io            :as io]
            [clojure.spec               :as s]
            [clj-time.core              :as t]
            [clj-time.format            :as f]
            [clj-time.local             :as l]
            [clojure.math.numeric-tower :as math]
            [seesaw.timer               :as st]
            [seesaw.core              :as sc]
            [seesaw.border            :as sb]
            [seesaw.graphics            :as sg]
            [seesaw.color               :as sclr]
            [seesaw.font                :as sf]
            [taoensso.timbre            :as timbre]
            [taoensso.timbre.appenders.core :as appenders])
    (:use seesaw.core)
    (:use seesaw.border)
    (:use seesaw.graphics)
    (:use seesaw.color)
    (:use seesaw.font)
    (:import (javax.swing JFrame JLabel)
           (java.awt Color Font FontMetrics GraphicsEnvironment)
           (java.io ByteArrayInputStream)))


(def landscape-pic (atom nil))

; fill a graphics context with color
(defn fill
  [^java.awt.Graphics2D g2d color width height]
  (do
      (.setPaint g2d color)
      (.fillRect g2d 0 0 width height)
      g2d))

; draw a list of lines
(defn draw-line-seq
  [^java.awt.Graphics2D g2d points draw-style]
  (doseq [pair (partition 2 1 points)]
    (sg/draw g2d
          (sg/line (-> pair first first)
              (-> pair first second)
              (-> pair second first)
              (-> pair second second))
          draw-style)))

; rotate an image around its center
(defn center-rotate
    [image angle]
    (let [width       (.getWidth image)
          height      (.getHeight image)
          neg         (fn [x] (- 0 x))
          buffer      (sg/buffered-image width height)]
        (-> buffer
          .createGraphics
          (sg/translate (/ width 2) (/ height 2))
          (sg/rotate angle)
          (sg/translate (neg (/ width 2)) (neg (/ height 2)))
          (sg/draw (sg/image-shape 0 0 image) nil))
         buffer))

; load an image and set it as background
(defn set-background
    []
    (let [bg-name  (utils/get-background-name)
          bg-image (utils/read-image bg-name)
          info-img (sun/inprint-image bg-image)]
        (reset! landscape-pic info-img)))

