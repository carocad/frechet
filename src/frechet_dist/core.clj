(ns frechet-dist.core
  (:require [clojure.core.matrix :refer [distance]]
            [frechet-dist.partial :refer [relax-boundaries]]
            [frechet-dist.shared :refer [link-matrix find-sequence point-distance]]))

(defn frechet-dist
  "calculate the discrete frechet distance between two curves.
  P and Q can be MxD and NxD matrixes. This means that both MUST
  have the same number of columns but need not have the same amount of
  rows.
  dist-fn is a function to evaluate the distance between any two rows
  of P and Q. It defaults to the Euclidean distance"
  ([P Q]
   (frechet-dist P Q distance))
  ([P Q dist-fn]
  (let [p2p-dist   (point-distance P Q dist-fn)
        [dist CA]  (link-matrix p2p-dist)
        coupling   (find-sequence CA)]
    [dist coupling])))

(defn partial-frechet-dist
  "compute the partial frechet distance among P and Q. The partial distance is
  calculated as the frechet distance among R and T, where R and T are R and T
  are the longest continous subcurves from P and Q that minimize the frechet
  distance"
  ([P Q]
   (partial-frechet-dist P Q distance))
  ([P Q dist-fn]
   (let [p2p-dist      (point-distance P Q dist-fn)
         limits        (relax-boundaries p2p-dist)
         [dist CA]     (link-matrix p2p-dist limits)
         coupling      (find-sequence CA limits)]
     [dist coupling])))
