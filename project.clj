(defproject murth "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.numeric-tower "0.0.2"]
                 [org.clojure/core.match "0.2.0"]
                 [instaparse "1.2.2"]]
  :main murth.core
  :aot [murth.core])
