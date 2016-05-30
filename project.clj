(defproject frechet-dist "0.9.0"
  :description "Calculate the discrete Frechet distance between two polygonal curves"
  :url "https://github.com/carocad/frechet-dist"
  :license {:name "LGPL v3"
            :url "https://raw.githubusercontent.com/carocad/frechet-dist/master/LICENSE"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 ; excludes the inner clojure dependency of net.mikera/vectorz-clj
                 [net.mikera/vectorz-clj "0.43.1" :exclusions [org.clojure/clojure]]]
  :profiles {:dev
     {:dependencies [[org.clojure/clojure "1.7.0"]
                     [org.clojure/test.check "0.9.0"]
                     ;profiler
                     [com.taoensso/timbre "4.3.1"]]}})
