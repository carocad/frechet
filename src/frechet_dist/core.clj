(ns frechet-dist.core
  (:require [clojure.core.matrix :as matrix]
            [frechet-dist.partial :as partial]
            [frechet-dist.shared :as common]))

;; TODO: replace the generative testing with Clojure's Spec
;; TODO: check viability of replacing core.matrix with a simple vector of vector
;; TODO: check viability of replacing the CA computation with an approach similar to
;;       https://github.com/LightTable/Clojure/pull/94/files))
;; TODO: check viability of providing an N to 1 frechet distance by allocating
;;       a single p2p-dist and CA matrix that could be reused for the computation

(defn distance
  "calculate the discrete frechet distance between two curves.
  P and Q can be MxD and NxD matrixes. This means that both MUST
  have the same number of columns but need not have the same amount of
  rows.
  dist-fn is a function to evaluate the distance between any two rows
  of P and Q. It defaults to the Euclidean distance"
  ([P Q] (distance P Q matrix/distance))
  ([P Q dist-fn]
   (let [p2p-dist   (common/point-distance (matrix/coerce :vectorz P)
                                           (matrix/coerce :vectorz Q)
                                           dist-fn)
         link       (common/link-matrix p2p-dist)
         coupling   (common/find-sequence (:CA link))]
     {:dist (:dist link) :couple coupling})))

(defn partial-distance
  "compute the partial frechet distance among P and Q. The partial distance is
  calculated as the frechet distance among R and T, where R and T are R and T
  are the longest continous subcurves from P and Q that minimize the frechet
  distance.
  dist-fn is a function to evaluate the distance between any two rows
  of P and Q. It defaults to the Euclidean distance"
  ([P Q] (partial-distance P Q matrix/distance))
  ([P Q dist-fn]
   (let [p2p-dist      (common/point-distance (matrix/coerce P)
                                              (matrix/coerce Q)
                                              dist-fn)
         [starts ends] (partial/find-boundaries p2p-dist)
         all-bounds    (map #(apply concat %) (partial/cartesian starts ends))
         bounds        (filter partial/valid-bounds? all-bounds)
         frechets      (partial/part-curve-dist p2p-dist bounds)]
     (apply min-key :dist frechets))))
