(defproject smhi "0.1.0-SNAPSHOT"
  :description "SMHI weather clock"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
  				 [http.async.client "1.2.0"]
  				 [seesaw "1.4.6-SNAPSHOT"]
  				 [org.clojure/data.json "0.2.6"]
  				 [org.clojure/math.numeric-tower "0.0.4"]
  				 [clj-time "0.12.0"]]
  :main ^:skip-aot smhi.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
