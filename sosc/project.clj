(defproject sosc "0.1.0-SNAPSHOT"
  :description "DataWeek2020: The Science of Second Chances, A lisp programmer's
exploration into data literacy, oppertunity, discrimination and second chance candidates."
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [org.clojure/data.csv "1.0.0"]
                 [clojure-interop/java.io "1.0.5"]]
  :main ^:skip-aot sosc.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
