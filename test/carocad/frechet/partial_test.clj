(ns carocad.frechet.partial-test
  (:require [carocad.frechet.core :as frechet]
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

(comment
  In general the partial Frechét distance is the Frechet distance applied
  to two subcurves taken from the original curves. Thus all the properties
  of the Frechet distance apply\; except the boundary conditions)


; -------------------------------------------------------------------
; The frechet distance is simmetric, thus the order of the comparison
; doesn't matter for any two curves
; Ddf(P,Q) = Ddf(Q, P)
(defspec partial-simmetry
  300; tries
  (prop/for-all [P curve
                 Q curve]
    (= (:dist (frechet/partial-distance P Q frechet/euclidean))
       (:dist (frechet/partial-distance Q P frechet/euclidean)))))
;(tc/quick-check 100 partial-simmetry)


; -------------------------------------------------------------------
(comment The partial frechet distance is NOT a true metric, thus the triangle innequality
 does not hold. This is due to the fact that the part of the curves that matches
 and from which the distance is extracted, are not necessarily the same for the
 three comparisons.)

; -------------------------------------------------------------------
; Neither the 'dog' nor the 'man' are able to backtrace on the path they follow
; thus the coupling sequence must be monotonically increasing
; for example: ([0 0] [0 1] [1 1] [1 2])
(defspec partial-monotonicity
  100; tries
  (prop/for-all [P curve
                 Q curve]
    (let [frechet (frechet/partial-distance P Q frechet/euclidean)]
      (and (apply <= (map first (:couple frechet)))
           (apply <= (map second (:couple frechet)))))))
;(tc/quick-check 100 monotonicity-property)


; -------------------------------------------------------------------
; If the distance of two curves is 0, then the two curves are the same
; Ddf(P,Q) = 0 if P = Q
(defspec partial-equality
  100; tries
  (prop/for-all [P curve]
    (= (:dist (frechet/partial-distance P P frechet/euclidean)) 0.0)))
;(tc/quick-check 100 equality-property)


(comment The boundary condition is not fulfilled in the partial discrete Frechet
  distance since the boundaries are relaxed)
