(ns carocad.frechet
  (:require [carocad.frechet.partial :as partial]
            [carocad.frechet.shared :as common]))

;(set! *warn-on-reflection* true)
(defn euclidean
  "compute the Euclidean distance between two points. Both points
  must be N dimensional"
  [point1 point2]
  (let [length     (dec (count point1))
        square-sum (loop [counter 0
                          result  0.0]
                     (let [pow1   (get point1 counter)
                           pow2   (get point2 counter)
                           result (+ result (Math/pow (- pow1 pow2) 2))]
                       (if (= counter length)
                         result
                         (recur (inc counter) result))))]
    (Math/sqrt square-sum)))

(defn distance
  "Compute the discrete frechet distance between two curves. P and Q MUST be
  vectors; as well as each point (Pi, Qi) on them. Example: [[1 2],[3 4]]
  dist-fn is a function used to evaluate the distance between any two
  points of P and Q."
  [P Q dist-fn]
  (let [[_ _ i j :as bounds] [0 0 (dec (count P)) (dec (count Q))]
        link-matrix (common/link-matrix P Q dist-fn bounds)
        total       (common/get2D link-matrix [i j])
        coupling    (common/find-sequence link-matrix bounds)]
    {::distance total ::couple coupling}))

(defn partial-distance
  "Compute the partial frechet distance among P and Q. The partial distance is
  calculated as the frechet distance among R and T, where R and T are the longest
  continuous sub-curves from P and Q that minimize the frechet distance.
  dist-fn is a function used to evaluate the distance between any two
  points of P and Q."
  [P Q dist-fn]
  (let [p2p-dist     (common/point-distance P Q dist-fn)
        [starts ends] (partial/find-boundaries p2p-dist)
        all-bounds   (map #(apply concat %) (partial/cartesian starts ends))
        valid-bounds (filter partial/valid-bounds? all-bounds)
        frechets     (partial/part-curve-dist P Q dist-fn valid-bounds)]
    (apply min-key ::distance frechets)))
