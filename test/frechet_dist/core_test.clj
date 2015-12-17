(ns frechet-dist.core-test
  (:require [frechet-dist.core :refer [frechet-dist partial-frechet-dist]]
            [frechet-dist.sampler :refer [refine]]
            [clojure.core.matrix :refer [row-count]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def dimension (gen/sample (gen/choose 2 5) 1))
; a point is a collection of n-dimensional numbers
(def point     (gen/vector (gen/double* {:infinite? false :NaN? false :min -100 :max 100})
                           (first dimension)))
; a curve is a collection of 2 or more points
(def curve     (gen/vector point 2 20))


; -------------------------------------------------------------------
; The frechet distance is simmetric, thus the order of the comparison
; doesn't matter for any two curves
; Ddf(P,Q) = Ddf(Q, P)
(defspec simmetry-property
  100; tries
  (prop/for-all [P curve
                 Q curve]
    (= (first (frechet-dist P Q)) (first (frechet-dist Q P)))))
;(tc/quick-check 100 simmetry-property)


; -------------------------------------------------------------------
; The frechet distance is a true metric, thus the triangle-innequality
; holds for any 3 curves
; Ddf(P,Q) <= Ddf(P,R) + Ddf(R,Q)
(defspec triangle-innequality
  100; tries
    (prop/for-all [P curve
                   Q curve
                   R curve]
      (<= (first (frechet-dist P Q)) (+ (first (frechet-dist P R))
                                        (first (frechet-dist R Q))))))
;(tc/quick-check 100 triangle-innequality)


; -------------------------------------------------------------------
; Neither the 'dog' nor the 'man' are able to backtrace on the path they follow
; thus the coupling sequence must be monotonically increasing
; for example: ([0 0] [0 1] [1 1] [1 2])
(defspec monotonicity-property
  100; tries
  (prop/for-all [P curve
                 Q curve]
    (and (apply <= (map first (second (frechet-dist P Q))))
         (apply <= (map second (second (frechet-dist P Q)))))))
;(tc/quick-check 100 monotonicity-property)


; -------------------------------------------------------------------
; If the distance of two curves is 0, then the two curves are the same
; Ddf(P,Q) = 0 if P = Q
(defspec equality-property
  100; tries
  (prop/for-all [P curve]
    (= (first (frechet-dist P P)) 0.0)))
;(tc/quick-check 100 equality-property)


; -------------------------------------------------------------------
; The coupling sequence MUST be such that the first and the last elements
; of both curves have to be included otherwise one of the curves was not
; traverse completely
(defspec boundaries-condition
  100; tries
  (prop/for-all [P curve
                 Q curve]
    (and (= (first (second (frechet-dist P Q))) [0 0])
         (= (last  (second (frechet-dist P Q))) [(dec (row-count P)) (dec (row-count Q))]))))
;(tc/quick-check 100 boundaries-condition)
