(ns frechet-dist.profiler
  (:require [frechet-dist.core :refer [frechet-dist partial-frechet-dist]]
            [frechet-dist.sampler :refer [refine]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.core.matrix :refer [distance]]
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
    (= (:dist (frechet-dist P Q)) (:dist (frechet-dist Q P)))))

(def partial-distance
  (prop/for-all [P curve
                 Q curve]
    (= (:dist (partial-frechet-dist P Q)) (:dist (partial-frechet-dist Q P)))))

(def refine-distance
  (prop/for-all [P curve
                 Q curve]
    (let [distPij     (apply max (map distance P (rest P)))
          distQij     (apply max (map distance Q (rest Q)))
          D-max       (max distPij distQij)]
      (>= (:dist (frechet-dist P Q))
          (:dist (frechet-dist (refine P (/ D-max 3))
                               (refine Q (/ D-max 3))))))))

;; (profiler/profile :info :FRECHET (tc/quick-check 100 normal-distance))
;; (profiler/profile :info :PARTIAL-FRECHET (tc/quick-check 100 partial-distance))
;; (profiler/profile :info :REFINEMENT      (tc/quick-check 100 refine-distance))
