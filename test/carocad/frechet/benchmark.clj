(ns carocad.frechet.benchmark
  (:require [clojure.test :as test]
            [clojure.test.check :as check]
            [criterium.core :as criterium]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [carocad.frechet.core-test :as fretest]
            [carocad.frechet.partial-test :as part-fretest])
  (:import (java.io PushbackReader)))

(def version (with-open [reader (io/reader "project.clj")]
               (let [content (edn/read (PushbackReader. reader))]
                 (nth content 2))))

(def report-filename (str "resources/benchmark/" version ".txt"))

(defn report
  [generator-cases]
  (with-open [writer (io/writer report-filename)]
    (binding [*out* writer]
      (time (doseq [[generator message] generator-cases]
              (println (str "Benchmark: " message))
              (criterium/quick-bench (check/quick-check 5 generator)
                                     :os :runtime :verbose)
              (newline)
              (newline))))))

(test/deftest ^:benchmark parsing
  (report {fretest/symmetry-property "Frechet distance with Euclidean metric"
           part-fretest/partial-symmetry "Partial Frechet distance with Euclidean metric"})
  (println (slurp report-filename)))
