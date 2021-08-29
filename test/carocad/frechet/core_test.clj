(ns carocad.frechet.core-test
  (:require [carocad.frechet :as frechet]
    ;[clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test :as test]))

(def dimension (first (gen/sample (gen/choose 2 5))))
; a point is a collection of n-dimensional numbers
(def point (gen/vector (gen/double* {:infinite? false :NaN? false :min -100 :max 100})
                       dimension))
; a curve is a collection of 2 or more points
(def curve (gen/vector point 3 20))

(def last-index (comp dec count))

(defn almost<=
  "check if two numbers are almost less than or equal to. If they are less than
  then nothing is done, otherwise the relative difference is computed and
  compared with eps. Returns true if x is less than y or almost equal to it."
  [eps x y]
  (if (< x y) true                                          ; nothing to do if they are strictly less
              (> eps (/ (Math/abs (- x y))
                        (max (Math/abs x) (Math/abs y))))))


; -------------------------------------------------------------------
; The frechet distance is simmetric, thus the order of the comparison
; doesn't matter for any two curves
; Ddf(P,Q) = Ddf(Q, P)
(defspec simmetry-property
  300                                                       ; tries
  (prop/for-all [P curve
                 Q curve]
    (= (::frechet/distance (frechet/distance P Q frechet/euclidean))
       (::frechet/distance (frechet/distance Q P frechet/euclidean)))))
;(tc/quick-check 100 simmetry-property)


; -------------------------------------------------------------------
; The frechet distance is a true metric, thus the triangle-innequality
; holds for any 3 curves
; Ddf(P,Q) <= Ddf(P,R) + Ddf(R,Q)
;; WARNING: due to the floating point precision problem of computers, it is
;;          not possible to test strict less than but rather an approximation
(defspec triangle-innequality
  300                                                       ; tries
  (prop/for-all [P curve
                 Q curve
                 R curve]
    (almost<= 0.00001 (::frechet/distance (frechet/distance P Q frechet/euclidean))
              (+ (::frechet/distance (frechet/distance P R frechet/euclidean))
                 (::frechet/distance (frechet/distance R Q frechet/euclidean))))))
;(tc/quick-check 100 triangle-innequality)


; -------------------------------------------------------------------
; Neither the 'dog' nor the 'man' are able to backtrace on the path they follow
; thus the coupling sequence must be monotonically increasing
; for example: ([0 0] [0 1] [1 1] [1 2])
(defspec monotonicity-property
  300                                                       ; tries
  (prop/for-all [P curve
                 Q curve]
    (let [frechet (frechet/distance P Q frechet/euclidean)]
      (and (apply <= (map first (::frechet/couple frechet)))
           (apply <= (map second (::frechet/couple frechet)))))))
;(tc/quick-check 100 monotonicity-property)


; -------------------------------------------------------------------
; If the distance of two curves is 0, then the two curves are the same
; Ddf(P,Q) = 0 if P = Q
(defspec equality-property
  100                                                       ; tries
  (prop/for-all [P curve]
    (= (::frechet/distance (frechet/distance P P frechet/euclidean)) 0.0)))
;(tc/quick-check 100 equality-property)


; -------------------------------------------------------------------
; The coupling sequence MUST be such that the first and the last elements
; of both curves have to be included otherwise one of the curves was not
; traverse completely
(defspec boundaries-condition
  100                                                       ; tries
  (prop/for-all [P curve
                 Q curve]
    (and (= (first (::frechet/couple (frechet/distance P Q frechet/euclidean))) [0 0])
         (= (last (::frechet/couple (frechet/distance P Q frechet/euclidean))) [(last-index P) (last-index Q)]))))
;(tc/quick-check 100 boundaries-condition)

(test/deftest unit-test
  (let [C1 [[10.0 0] [8.7 0.5] [8 1.3] [7 2.4] [7.6 2.8] [8.3 3.4] [8.9 4.0] [9 4.8] [8.3 5] [7.6 5.2] [6.4 5.8] [6.3 6.6] [7 7] [6.1 7.3] [5 7]]
        C2 [[8.5 4.7] [7.3 5.4] [6.2 5.6] [6.4 6.6] [6.4 7] [5.8 6.7] [5.5 7.1] [4.8 6.9] [4.4 7] [3.4 8.1] [4 8.3] [3.8 7.2] [3.6 6] [3.4 4.6]]]
    (test/testing "frechet distance"
      (let [result (frechet/distance C1 C2 frechet/euclidean)]
        (test/is (= result #::frechet{:distance 4.933558553417604,
                                      :couple   '([0 0]
                                                  [1 0]
                                                  [2 1]
                                                  [3 2]
                                                  [4 3]
                                                  [5 4]
                                                  [6 5]
                                                  [7 6]
                                                  [8 7]
                                                  [9 8]
                                                  [10 9]
                                                  [11 10]
                                                  [12 11]
                                                  [13 12]
                                                  [14 13])}))))
    (test/testing "partial frechet distance"
      (let [result (frechet/partial-distance C1 C2 frechet/euclidean)]
        (test/is (= result #::frechet{:dist   0.6708203932499366,
                                      :couple '([8 0]
                                                [9 1]
                                                [10 2]
                                                [11 3]
                                                [12 4]
                                                [13 5]
                                                [13 6]
                                                [14 7])}))))))


