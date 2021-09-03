(ns carocad.frechet.test-runner
  (:require [clojure.test :as test]
            ;; https://clojurescript.org/tools/testing#running-tests
            [carocad.frechet.core-test :as core]
            [carocad.frechet.partial-test :as partial]))

;; https://clojurescript.org/guides/quick-start#running-clojurescript-on-node.js
(defn -main []
  (enable-console-print!)
  (test/run-all-tests #"carocad.frechet.+"))

;(-main)
