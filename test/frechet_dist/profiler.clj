(ns frechet-dist.profiler
  (:require [frechet-dist.core :as frechet]
            [frechet-dist.sampler :as sampler]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.core.matrix :as matrix]
            [taoensso.timbre.profiling :as profiler]))
            ;[clojure.test.check.clojure-test :refer [defspec]]))

(def dimension (gen/sample (gen/choose 2 5) 1))
; a point is a collection of n-dimensional numbers
(def point     (gen/vector (gen/double* {:infinite? false :NaN? false :min -100 :max 100})
                           (first dimension)))
; a curve is a collection of 2 or more points
(def curve     (gen/vector point 50 300))

(def last-index (comp dec count))

(def normal-distance
  (prop/for-all [P curve
                 Q curve]
    (= (:dist (frechet/distance P Q frechet/euclidean))
       (:dist (frechet/distance Q P frechet/euclidean)))))

(def partial-distance
  (prop/for-all [P curve
                 Q curve]
    (= (:dist (frechet/partial-distance P Q frechet/euclidean))
       (:dist (frechet/partial-distance Q P frechet/euclidean)))))

(def refine-distance
  (prop/for-all [P curve
                 Q curve]
    (let [distPij     (apply min (map matrix/distance P (rest P)))
          distQij     (apply min (map matrix/distance Q (rest Q)))
          D-min       (min distPij distQij)]
      (>= (:dist (frechet/distance P Q frechet/euclidean))
          (:dist (frechet/distance (sampler/refine P D-min)
                                   (sampler/refine Q D-min)
                                   frechet/euclidean))))))

;; (profiler/profile :info :FRECHET (tc/quick-check 100 normal-distance))
;; (profiler/profile :info :PARTIAL-FRECHET (tc/quick-check 100 partial-distance))
;; (profiler/profile :info :REFINEMENT      (tc/quick-check 100 refine-distance))
