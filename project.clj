(defproject aoc "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/core.logic  "1.0.0"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/data.int-map "1.0.0"]
                 [org.clojure/data.priority-map "1.1.0"]
                 [instaparse "1.4.12"]]
  :main ^:skip-aot aoc.core
  :jvm-opts ["-Xmx16g"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
