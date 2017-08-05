(ns smhi.logfile
    (:require 	(smhi         [utils    :refer :all])
                (clojure.java [io       :as io])
            	(clojure 	  [string   :as str])
              	(taoensso 	  [timbre   :as log])
               	(taoensso.timbre.appenders 	[core :as appenders])
))

;;-----------------------------------------------------------------------------

(defn- rotate-file
    [log path prefix num-files]
    (let [dir     (get-dir-list path (re-pattern (str prefix "-\\d+\\.log")))
          extract (fn [x] (str/replace x #"^([^-]+-)([0-9]+)(\.log)$" "$2"))
          numbers (map extract dir)
          biggest (->> numbers (map #(Integer/valueOf %)) sort last)
          new-big (str path prefix "-" (if (nil? biggest) 0 (inc biggest)) ".log")]
        (.renameTo log (io/file new-big))
        (.createNewFile log)))

(defn- max-size-appender
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
         :fn (fn [data]
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
                                (rotate-file log path prefix num-files)))
                            (.createNewFile log))
                        (spit filename (with-out-str (println output-str)) :append true)
                        (catch java.io.IOException _)))))})

(defn setup-log
    []
    (log/merge-config! {
        :level :trace
        :appenders {
            :println {:enabled? false}
            :spit
                (max-size-appender
                    {:path "./" :prefix "clock" :max-size 1000000 :num-files 2})}
        :timestamp-opts {
            :pattern "MM-dd HH:mm:ss"
            :locale (java.util.Locale. "sv_SE")
            :timezone (java.util.TimeZone/getTimeZone "Europe/Stockholm")}
        :output-fn (partial log/default-output-fn {:stacktrace-fonts {}})}))

