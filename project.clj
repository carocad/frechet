(defproject net.clojars.carocad/frechet "0.14.0"
  :description "Calculate the discrete Frechet distance between two polygonal curves"
  :url "https://github.com/carocad/frechet-dist"
  :license {:name "LGPL v3"
            :url  "https://github.com/carocad/frechet-dist/blob/master/LICENSE"}
  :dependencies [[org.clojure/clojure "1.10.1" :scope "provided"]
                 [org.clojure/clojurescript "1.10.758" :scope "provided"]]
  :resource-paths [];; don't include scripts nor benchmarks in the jar
  :profiles {:dev {:dependencies [[criterium "0.4.5"]
                                  [org.clojure/test.check "0.10.0"]]
                   :test-selectors {:default     (fn [m] (not (some #{:benchmark} (keys m))))
                                    :benchmark   :benchmark}
                   :plugins [[jonase/eastwood "0.3.5"]]}}
  ;; deploy to clojars as - lein deploy releases
  :deploy-repositories [["clojars" {:url "https://clojars.org/repo"
                                    :username :env/clojars_username
                                    :password :env/clojars_password
                                    :sign-releases false}]])
