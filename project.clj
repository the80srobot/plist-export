(defproject org.clojars.the80srobot/plist-export "0.0.1"
  :description "Clojure lazy plist library with import/export support."
  :dependencies [[org.clojure/clojure "1.4.0"]
                  [commons-codec/commons-codec "1.4"]]
  :url "https://github.com/the80srobot/plist-export"
  :aot [plist-export.export]
  :license {:name "Eclipse Public License - v 1.0"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "Same license as Clojure."})