(defproject frechet-dist "0.4.7"
  :description "Calculate the discrete Frechet distance between two polygonal curves"
  :url "https://github.com/carocad/frechet-dist"
  :license {:name "LGPL v3"
            :url "https://raw.githubusercontent.com/carocad/frechet-dist/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [net.mikera/vectorz-clj "0.37.0" :exclusions [org.clojure/clojure]]
                 ; excludes the inner clojure dependency of net.mikera/vectorz-clj
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0"]
                                  [org.clojure/test.check "0.9.0"]]}})
