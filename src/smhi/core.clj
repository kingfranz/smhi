(ns smhi.core
    (:require   (smhi     [utils       :refer :all]
                          [config      :refer :all]
                          [logfile     :refer :all]
                          [graph-utils :refer :all]
                          [frame       :refer :all]
                          [sun         :refer :all]
                          [wnow        :refer :all]
                          [radar       :refer :all]
                          [forecast    :refer :all]
                          [weather     :refer :all]
                          [images      :refer :all])
                (seesaw   [timer       :as st]
                          [dev         :as sd]
                          [graphics    :as sg]
                          [core        :as sc])
                (taoensso [timbre      :as log]))
    (:gen-class))

;;-----------------------------------------------------------------------------

(defn set-screen
    [the-frame args]
    (let [num-args   (count args)
          arg-num    (if (and (> num-args 0) (is-pos-int-str? (first args)))
                        (Integer/valueOf (first args))
                        0)
          screens    (get-screens)
          screen-num (if (>= arg-num (count screens))
                        0
                        arg-num)]
        (sc/move! the-frame :to [(:x (nth screens screen-num)) (:y (nth screens screen-num))])))

(defn setup-config
    []
    (read-config-file)
;    (log/info "starting watch")
;    (let [changes (aw/changes-in ["./"])]
;        (async/go (while true
;            (let [[op filename] (<! changes)]
;                ;; op will be one of :create, :modify or :delete
;                (if (and (= op :modify) (= filename "./smhi-config.edn"))
;                    (do
;                        (log/info "smhi-config.edn updated")
;                        (read-config-file)
;                        (sc/repaint! (smhi-frame))))))))
;    (log/info "after watch"))
)

(defn maybe
  	[g2d func width height opt]
    (if (nil? opt)
		(func g2d width height)
 		(func g2d width height opt)))

(defn inprint
  	[g2d tag func & opt]
   	(let [widget (sc/select (smhi-frame) [tag])
          width  (.getWidth widget)
          height (.getHeight widget)
          left-x (.getX widget)
          top-y  (.getY widget)]
      	(sg/push g2d
                 (as-> g2d $
                       (sg/translate $ left-x top-y)
                       (apply func $ width height opt)))))

(defn -main
    [& args]
    ;(sd/debug!)
    (setup-log)
    (log/info "========= Starting up =========")    
    (setup-config)
    (setup-images)
    
    (-> (smhi-frame) (set-screen args) sc/show!)

    ; set timer for clock refresh
    (st/timer (fn [_]
        (sc/repaint! (sc/select (smhi-frame) [:#clock])))
        :initial-delay (* 1000 (config :clock-delay-sec))
        :delay (/ 1000 (config :clock-fps)))

    ; set timer for radar image paint
    (st/timer (fn [_]
        (sc/repaint! (sc/select (smhi-frame) [:#radar])))
        :initial-delay (* 1000 (config :radar-ani-delay-sec))
        :delay (/ 1000 (config :radar-fps)))

    ; set timer for weather forecast
;    (st/timer  (fn [_]
;      	(let [widget (sc/select (smhi-frame) [:#forecast])
;          	  width  (.getWidth widget)
;          	  height (.getHeight widget)]
;             (weather-update width height)))
;        :initial-delay (* 1000 (config :weather-timer-initial-sec))
;        :delay (* 60 1000 (config :weather-timer-delay-min)))

    ; set timer for radar image download
    (st/timer (fn [_]
        (radar-timer-fn)
        (let [widget (sc/select (smhi-frame) [:#forecast])
          	  width  (.getWidth widget)
          	  height (.getHeight widget)
              bkgrnd (get-background)
              bg-g2d (.createGraphics bkgrnd)]
            (weather-update width height)
          	(inprint bg-g2d :#clock 			  draw-image (clock-pics :clock-pic) :min :center :center "clock")
            (inprint bg-g2d :#clock 			  write-sun-info)
            (inprint bg-g2d :#left-axis 		  draw-left-axis)
            (inprint bg-g2d :#right-axis 		  draw-right-axis)
           	(doseq [day-idx (range (config :graph-days))]
	         	(inprint bg-g2d (->> day-idx (str "#day-") keyword) draw-day day-idx))
            (inprint bg-g2d :#forecast 			  draw-forecast)
            (inprint bg-g2d :#wnow-temp 		  draw-wnow-temp)
            (inprint bg-g2d :#wnow-humidity 	  draw-wnow-humidity)
            (inprint bg-g2d :#wnow-cloud 		  draw-wnow-cloud)
            (inprint bg-g2d :#wnow-wind 		  draw-wnow-wind)
            (inprint bg-g2d :#wnow-symbol 		  draw-wnow-symbol)
            (inprint bg-g2d :#wnow-baro 		  draw-wnow-baro)
            (inprint bg-g2d :#wnow-rain 		  draw-wnow-rain)
            (inprint bg-g2d :#wnow-thunder 		  draw-wnow-thunder)
            (inprint bg-g2d :#wnow-direction-txt  draw-wnow-direction-txt)
            (inprint bg-g2d :#wnow-direction-symb draw-wnow-direction-symb)
            ;(write-image "test.png" bkgrnd)
            (set-background bkgrnd)
            (sc/repaint! (sc/select (smhi-frame) [:#lbl-back]))))
        :initial-delay (* 1000 (config :radar-timer-initial-sec))
        :delay (* 60 1000 (config :radar-interval-minutes)))
    )

