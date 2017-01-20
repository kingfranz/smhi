(ns smhi.graph-utils
  (:require [smhi.utils            :refer :all])
  (:require [smhi.sun            :refer :all])
  (:require [clojure.java.io            :as io])
  (:require [clj-time.core              :as t])
  (:require [clj-time.format            :as f])
  (:require [clj-time.local             :as l])
  (:require [clojure.math.numeric-tower :as math])
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
    (draw g2d
          (line (-> pair first first)
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
            buffer      (buffered-image width height)]
         (-> buffer
          .createGraphics
          (translate (/ width 2) (/ height 2))
          (rotate angle)
          (translate (neg (/ width 2)) (neg (/ height 2)))
          (draw (image-shape 0 0 image) nil))
         buffer))

; load an image and set it as background
(defn set-background
    []
    (let [bg-name  (get-background-name)
          bg-image (read-image bg-name)
          info-img (inprint-image bg-image)]
        (set-var landscape-pic info-img)))

