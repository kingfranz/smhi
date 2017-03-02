(defproject smhi "1.0.0"
  :description "SMHI weather clock"
  :url "http://smhi.se"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha14"]
                 [seesaw "1.4.6-SNAPSHOT"]
                 [com.taoensso/timbre "4.8.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [http-kit "2.2.0"]
                 [org.clojure/data.xml "0.2.0-alpha2"]
                 [org.clojure/data.zip "0.1.2"]
                 [async-watch "0.1.1"]
                 [clj-time "0.13.0"]]
  :main ^:skip-aot smhi.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
