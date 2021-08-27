(ns carocad.frechet.benchmark
  (:require [clojure.test :as test]
            [clojure.test.check.properties :as properties]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as generators]
            [criterium.core :as criterium]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [carocad.frechet.core :as frechet]
            [clojure.core.matrix :as matrix]
            [carocad.frechet.sampler :as sampler])
  (:import (java.io PushbackReader)))

(def dimension (generators/sample (generators/choose 2 5) 1))
; a point is a collection of n-dimensional numbers
(def point (generators/vector (generators/double* {:infinite? false :NaN? false :min -100 :max 100})
                              (first dimension)))
; a curve is a collection of 2 or more points
(def curve (generators/vector point 50 300))

(def normal-distance
  (properties/for-all [P curve
                       Q curve]
    (= (:dist (frechet/distance P Q frechet/euclidean))
       (:dist (frechet/distance Q P frechet/euclidean)))))

(def partial-distance
  (properties/for-all [P curve
                       Q curve]
    (= (:dist (frechet/partial-distance P Q frechet/euclidean))
       (:dist (frechet/partial-distance Q P frechet/euclidean)))))

(def refine-distance
  (properties/for-all [P curve
                       Q curve]
    (let [distPij (apply min (map matrix/distance P (rest P)))
          distQij (apply min (map matrix/distance Q (rest Q)))
          D-min   (max distPij distQij)]
      (>= (:dist (frechet/distance P Q frechet/euclidean))
          (:dist (frechet/distance (sampler/refine P D-min)
                                   (sampler/refine Q D-min)
                                   frechet/euclidean))))))

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
  (report {normal-distance "Frechet distance with Euclidean metric"
           partial-distance "Partial Frechet distance with Euclidean metric"
           refine-distance "Frechet distance with interpolated points"})
  (println (slurp report-filename)))
