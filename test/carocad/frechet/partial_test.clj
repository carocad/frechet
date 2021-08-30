(ns carocad.frechet.partial-test
  (:require [carocad.frechet :as frechet]
            [clojure.test :as test]
            [clojure.pprint :as pprint]
            [clojure.test.check :as check]
            [clojure.test.check.generators :as generator]
            [clojure.test.check.properties :as property]))

(def dimension (first (generator/sample (generator/choose 2 5))))
; a point is a collection of n-dimensional numbers
(def point (generator/vector (generator/double* {:infinite? false :NaN? false :min -100 :max 100})
                             dimension))
; a curve is a collection of 2 or more points
(def curve (generator/vector point 3 300))

(comment
  In general the partial Frechét distance is the Frechet distance applied
  to two sub-curves taken from the original curves. Thus all the properties
  of the Frechet distance apply \; except the boundary conditions)


(def partial-symmetry
  "The frechet distance is symmetric, thus the order of the comparison
   doesn't matter for any two curves
   Ddf(P,Q) = Ddf(Q, P)"
  (property/for-all [P curve
                     Q     curve]
    (= (::frechet/distance (frechet/partial-distance P Q frechet/euclidean))
       (::frechet/distance (frechet/partial-distance Q P frechet/euclidean)))))
;(tc/quick-check 100 partial-symmetry)

(comment The partial frechet distance is NOT a true metric, thus the triangle inequality
         does not hold. This is due to the fact that the part of the curves that matches
         and from which the distance is extracted, are not necessarily the same for the
         three comparisons.)

(def partial-monotonicity
  "Neither the 'dog' nor the 'man' are able to backtrace on the path they follow
   thus the coupling sequence must be monotonically increasing
   for example: ([0 0] [0 1] [1 1] [1 2])"
  (property/for-all [P curve
                     Q     curve]
    (let [frechet (frechet/partial-distance P Q frechet/euclidean)]
      (and (apply <= (map first (::frechet/couple frechet)))
           (apply <= (map second (::frechet/couple frechet)))))))
;(tc/quick-check 100 monotonicity-property)

(def partial-equality
  "If the distance of two curves is 0, then the two curves are the same
   Ddf(P,Q) = 0 if P = Q"
  (property/for-all [P curve]
    (= (::frechet/distance (frechet/partial-distance P P frechet/euclidean)) 0.0)))
;(tc/quick-check 100 equality-property)


(comment The boundary condition is not fulfilled in the partial discrete Frechet
         distance since the boundaries are relaxed)

(test/deftest partial-frechet-generative-testing
  (let [result (check/quick-check 300 partial-symmetry)]
    (test/is (:pass? result))
    (when (not (:pass? result))
      (pprint/pprint result)))
  (let [result (check/quick-check 300 partial-monotonicity)]
    (test/is (:pass? result))
    (when (not (:pass? result))
      (pprint/pprint result)))
  (let [result (check/quick-check 300 partial-equality)]
    (test/is (:pass? result))
    (when (not (:pass? result))
      (pprint/pprint result))))
