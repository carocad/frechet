(ns frechet-dist.core
  (:require [clojure.core.matrix :as matrix :refer [distance]]
            [frechet-dist.partial :refer [find-boundaries valid-bounds? cartesian]]
            [frechet-dist.shared :refer [link-matrix find-sequence point-distance wrap]]))

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
  (let [p2p-dist   (point-distance (wrap P) (wrap Q) dist-fn)
        link       (link-matrix p2p-dist)
        coupling   (find-sequence (:CA link))]
    {:dist (:dist link) :couple coupling})))

(defn partial-frechet-dist
  "compute the partial frechet distance among P and Q. The partial distance is
  calculated as the frechet distance among R and T, where R and T are R and T
  are the longest continous subcurves from P and Q that minimize the frechet
  distance"
  ([P Q]
   (partial-frechet-dist P Q distance))
  ([P Q dist-fn]
   (let [p2p-dist      (point-distance (wrap P) (wrap Q) dist-fn)
         [starts ends] (find-boundaries p2p-dist)
         bounds        (filter valid-bounds? (map #(apply concat %)
                                                  (cartesian starts ends)))
         frechets      (map #(merge {:bounds %} (link-matrix p2p-dist %)) bounds)
         min-frecht    (apply min-key :dist frechets)
         coupling      (find-sequence (:CA min-frecht) (:bounds min-frecht))]
     {:dist (:dist min-frecht) :couple coupling})))
