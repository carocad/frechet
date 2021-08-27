(ns carocad.frechet
  (:require [clojure.core.matrix :as matrix]
            [carocad.frechet.partial :as partial]
            [carocad.frechet.shared :as common]
            [carocad.frechet.protocols :as proto]))

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
(def euclidean "euclidean distance for n-dimentional points" matrix/distance)

(defn distance
  "calculate the discrete frechet distance between two curves. P and Q can be
  arbirtrary sequential collections as long as either the
  carocad.frechet.protocols/distance protocol is implemented or a function to
  evaluate the distance between points is provided.
  dist-fn is an optional function used to evaluate the distance between any two
  points of P and Q. It defaults to carocad.frechet.protocols/distance"
  ([P Q]
   (distance P Q proto/distance))
  ([P Q dist-fn]
   (let [p2p-dist   (common/point-distance P Q dist-fn)
         link       (common/link-matrix p2p-dist)
         coupling   (common/find-sequence (:CA link))]
     {:dist (:dist link) :couple coupling})))

(defn partial-distance
  "compute the partial frechet distance among P and Q. The partial distance is
  calculated as the frechet distance among R and T, where R and T are the longest
  continous subcurves from P and Q that minimize the frechet distance.
  dist-fn is an optional function used to evaluate the distance between any two
  points of P and Q. It defaults to carocad.frechet.protocols/distance"
  ([P Q]
   (partial-distance P Q proto/distance))
  ([P Q dist-fn]
   (let [p2p-dist      (common/point-distance P Q dist-fn)
         [starts ends] (partial/find-boundaries p2p-dist)
         all-bounds    (map #(apply concat %) (partial/cartesian starts ends))
         bounds        (filter partial/valid-bounds? all-bounds)
         frechets      (partial/part-curve-dist p2p-dist bounds)]
     (apply min-key :dist frechets))))
