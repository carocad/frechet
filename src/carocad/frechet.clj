(ns carocad.frechet
  (:require [clojure.core.matrix :as matrix]
            [carocad.frechet.partial :as partial]
            [carocad.frechet.shared :as common]))

;; TODO: replace the generative testing with Clojure's Spec
;; TODO: check viability of replacing core.matrix with a simple vector of vector
;; TODO: check viability of replacing the CA computation with an approach similar to
;;       https://github.com/LightTable/Clojure/pull/94/files))
;; TODO: refactor library into (distance P Q), (coupling P Q) and (shortest-path P Q)
;; TODO: check viability of providing an N to 1 frechet distance by allocating
;;       a single p2p-dist and CA matrix that could be reused for the computation
;; TODO: remove the legacy namespace, there is no need for it anymore
;; TODO: refactor the refine function to be able to interpolate both vectors and
;;       hash-maps (provide a protocol for it ;)
;; TODO: refactor the README accordingly

;; reexposed here for convenience
(def euclidean "euclidean distance for n-dimensional points" matrix/distance)

(defn distance
  "Compute the discrete frechet distance between two curves. P and Q MUST be
  vectors; as well as each point (Pi, Qi) on them. Example: [[1 2],[3 4]]
  dist-fn is a function used to evaluate the distance between any two
  points of P and Q."
  [P Q dist-fn]
  (let [p2p-dist   (common/point-distance P Q dist-fn)
        link       (common/link-matrix p2p-dist)
        coupling   (common/find-sequence (:CA link))]
    {:dist (:dist link) :couple coupling}))

;(set! *warn-on-reflection* true)
(defn euclidean2
  [point1 point2]
  (let [length     (dec (count point1))
        square-sum (loop [counter 0
                          result 0.0]
                     (let [pow1 (get point1 counter)
                           pow2 (get point2 counter)
                           result (+ result (Math/pow (- pow1 pow2) 2))]
                       (if (= counter length)
                         result
                         (recur (inc counter) result))))]
    (Math/sqrt square-sum)))

(defn distance4
  [P Q dist-fn]
  (let [link-matrix (common/link-matrix2 P Q dist-fn)
        [_ _ i j :as bounds] (common/bounds link-matrix)
        total       (aget ^doubles (aget link-matrix i) j)
        coupling    (common/find-sequence2 link-matrix bounds)]
    {::distance total ::couple coupling}))

(let [P  [[3 2]
          [3 4]
          [5 6]]
      Q  [[1 2]
          [3 4]
          [5 6]]]
  ;(time (distance P Q euclidean))
  ;(newline)
  (time (distance4 P Q euclidean2))
  #_(time
      (dotimes [_ 10000000]
        (distance4 P Q euclidean2))))

(defn partial-distance
  "Compute the partial frechet distance among P and Q. The partial distance is
  calculated as the frechet distance among R and T, where R and T are the longest
  continuous sub-curves from P and Q that minimize the frechet distance.
  dist-fn is a function used to evaluate the distance between any two
  points of P and Q."
  [P Q dist-fn]
  (let [p2p-dist      (common/point-distance P Q dist-fn)
        [starts ends] (partial/find-boundaries p2p-dist)
        all-bounds    (map #(apply concat %) (partial/cartesian starts ends))
        bounds        (filter partial/valid-bounds? all-bounds)
        frechets      (partial/part-curve-dist p2p-dist bounds)]
    (apply min-key :dist frechets)))
