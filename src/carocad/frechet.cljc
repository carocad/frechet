(ns carocad.frechet
  (:require [carocad.frechet.partial :as partial]
            [carocad.frechet.shared :as common]))

;(set! *warn-on-reflection* true)
;(set! *warn-on-infer* true)

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
  "Compute the discrete frechet distance between two curves.

   - `P` and `Q` MUST be vectors; as well as each point (Pi, Qi)
   on them. Example: [[1 2],[3 4]].

   - `dist-fn` is a function used to evaluate the distance between any two
   points of P and Q."
  [P Q dist-fn]
  (let [[_ _ i j]   [0 0 (dec (count P)) (dec (count Q))]
        link-matrix (common/link-matrix P Q dist-fn)
        total       (common/get2D link-matrix [i j])
        coupling    (common/find-sequence link-matrix)]
    {::distance total ::coupling coupling}))

(defn partial-distance
  "Compute the partial frechet distance among P and Q. The partial distance is
  calculated as the frechet distance among R and T, where R and T are the longest
  continuous sub-curves from P and Q that minimize the frechet distance.

  - `P` and `Q` MUST be vectors; as well as each point (Pi, Qi)
  on them. Example: [[1 2],[3 4]]

  - `dist-fn` is a function used to evaluate the distance between any two
  points of P and Q.

  WARNING: The partial-frechet distance is not a true mathematical metric. It
  was created by me to support my thesis work. Use at your own risk 👾"
  [P Q dist-fn]
  (let [p2p-dist     (common/point-distance P Q dist-fn)
        [starts ends] (partial/find-boundaries p2p-dist)
        all-bounds   (map #(apply concat %) (partial/cartesian starts ends))
        valid-bounds (filter partial/valid-bounds? all-bounds)
        frechets     (partial/part-curve-dist P Q dist-fn valid-bounds)]
    (apply min-key ::distance frechets)))

#_(let [C1 [[10.0 0] [8.7 0.5] [8 1.3] [7 2.4] [7.6 2.8] [8.3 3.4] [8.9 4.0] [9 4.8] [8.3 5] [7.6 5.2] [6.4 5.8] [6.3 6.6] [7 7] [6.1 7.3] [5 7]]
        C2 [[8.5 4.7] [7.3 5.4] [6.2 5.6] [6.4 6.6] [6.4 7] [5.8 6.7] [5.5 7.1] [4.8 6.9] [4.4 7] [3.4 8.1] [4 8.3] [3.8 7.2] [3.6 6] [3.4 4.6]]]
    (partial-distance C1 C2 euclidean))

#_(let [P [[3 2]
           [3 4]
           [5 6]]
        Q [[1 2]
           [3 4]
           [5 6]]]
    (time (distance P Q euclidean))
    ;(newline)
    #_(time
        (dotimes [_ 100000000]
          (distance P Q euclidean))))
