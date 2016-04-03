(ns frechet-dist.core-test
  (:require [frechet-dist.core :refer [frechet-dist]]
            ;[clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]))

(def dimension (gen/sample (gen/choose 2 5) 1))
; a point is a collection of n-dimensional numbers
(def point     (gen/vector (gen/double* {:infinite? false :NaN? false :min -100 :max 100})
                           (first dimension)))
; a curve is a collection of 2 or more points
(def curve     (gen/vector point 2 20))

(def last-index (comp dec count))


; -------------------------------------------------------------------
; The frechet distance is simmetric, thus the order of the comparison
; doesn't matter for any two curves
; Ddf(P,Q) = Ddf(Q, P)
(defspec simmetry-property
  300; tries
  (prop/for-all [P curve
                 Q curve]
    (= (:dist (frechet-dist P Q)) (:dist (frechet-dist Q P)))))
;(tc/quick-check 100 simmetry-property)


; -------------------------------------------------------------------
; The frechet distance is a true metric, thus the triangle-innequality
; holds for any 3 curves
; Ddf(P,Q) <= Ddf(P,R) + Ddf(R,Q)
(defspec triangle-innequality
  300; tries
    (prop/for-all [P curve
                   Q curve
                   R curve]
      (<= (:dist (frechet-dist P Q)) (+ (:dist (frechet-dist P R))
                                        (:dist (frechet-dist R Q))))))
;(tc/quick-check 100 triangle-innequality)


; -------------------------------------------------------------------
; Neither the 'dog' nor the 'man' are able to backtrace on the path they follow
; thus the coupling sequence must be monotonically increasing
; for example: ([0 0] [0 1] [1 1] [1 2])
(defspec monotonicity-property
  300; tries
  (prop/for-all [P curve
                 Q curve]
    (let [frechet     (frechet-dist P Q)]
      (and (apply <= (map first (:couple frechet)))
           (apply <= (map second (:couple frechet)))))))
;(tc/quick-check 100 monotonicity-property)


; -------------------------------------------------------------------
; If the distance of two curves is 0, then the two curves are the same
; Ddf(P,Q) = 0 if P = Q
(defspec equality-property
  100; tries
  (prop/for-all [P curve]
    (= (:dist (frechet-dist P P)) 0.0)))
;(tc/quick-check 100 equality-property)


; -------------------------------------------------------------------
; The coupling sequence MUST be such that the first and the last elements
; of both curves have to be included otherwise one of the curves was not
; traverse completely
(defspec boundaries-condition
  100; tries
  (prop/for-all [P curve
                 Q curve]
    (and (= (first (:couple (frechet-dist P Q))) [0 0])
         (= (last  (:couple (frechet-dist P Q))) [(last-index P) (last-index Q)]))))
;(tc/quick-check 100 boundaries-condition)
