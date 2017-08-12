(defproject smhi "2.0.0"
  :description "SMHI weather clock"
  :url "http://smhi.se"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0-alpha17"]
                 [seesaw "1.4.5"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.clojure/data.json "0.2.6"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [http-kit "2.2.0"]
                 [image-resizer "0.1.10"]
                 [org.clojure/tools.reader "1.0.3"]
                 [org.clojure/data.zip "0.1.2"]
                 [async-watch "0.1.1"]
                 [clj-time "0.14.0"]]
  :main ^:skip-aot smhi.core
  :target-path "target/%s"
  :profiles {
        :dev {:jvm-opts ["-Dclojure.spec.compile-asserts=true"]}
        :uberjar {
            :jvm-opts ["-Dclojure.spec.compile-asserts=false"]
            :aot :all}}
  :plugins [[ns-graph "0.1.2"]]
  :jvm-opts ["-Dclojure.spec.compile-asserts=true"]
  :ns-graph {:name "SMHI project"
             :abbrev-ns true
             :source-paths (get-env :source-paths)
             :exclude ["java.*" "clojure.*"]})
