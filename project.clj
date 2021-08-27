(defproject frechet-dist "0.11.1"
  :description "Calculate the discrete Frechet distance between two polygonal curves"
  :url "https://github.com/carocad/frechet-dist"
  :license {:name "LGPL v3"
            :url  "https://github.com/carocad/frechet-dist/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [net.mikera/vectorz-clj "0.43.1" :exclusions [org.clojure/clojure]]]
  :profiles {:dev {:dependencies [[criterium "0.4.5"]
                                  [org.clojure/test.check "0.9.0"]]
                   :test-selectors {:default     (fn [m] (not (some #{:benchmark} (keys m))))
                                    :benchmark   :benchmark}
                   :plugins [[jonase/eastwood "0.2.3"]]}})
