(ns frechet-dist.partial
  (:require [clojure.core.matrix :refer [shape get-column get-row new-matrix mget]]
            [frechet-dist.shared :refer [bound-zero find-sequence compute-CA!]]))
            ;[taoensso.timbre.profiling :refer [defnp]]))

(defn valid-bounds?
  "check if the current bounds are valid. Valid bounds are such that the curve
  is reduced to a minimum of two points (with zero indexed bounds)"
  [[is js ie je]]
  (and (>= (- ie is) 1) (>= (- je js) 1)))

(defn- min-indexed
  "returns the a vector [index value] where index is the position at which the
  minimum value occurs in data."
  [data]
  (apply min-key second (map-indexed vector data)))

(defn- nearest-point
  "find which is the nearest point to i or j based on the distances given by
  p2p-dist."
  [p2p-dist i j]
  (cond
   (nil? i) (first (min-indexed (get-column p2p-dist j)))
   (nil? j) (first (min-indexed (get-row p2p-dist i)))))

(defn find-boundaries
  "find all nearest neighbours of the start and end points of each curve,
  including the default start and end of each curve. Only distinct start/end
  points are returned."
  [p2p-dist]
  (let [def-start             [0 0]
        si                    [0 (nearest-point p2p-dist 0 nil)]
        sj                    [(nearest-point p2p-dist nil 0) 0]
        [i_n j_n :as def-end] (subvec (vec (bound-zero p2p-dist)) 2)
        ei                    [i_n (nearest-point p2p-dist i_n nil)]
        ej                    [(nearest-point p2p-dist nil j_n) j_n]]
    [(distinct [def-start si sj]) (distinct [def-end ei ej])]))

(defn part-curve-dist
  "calculate the link distance and the coupling sequence of all possible
  boundaries (bounds). Returns all the partial frechet distances and their
  couplings"
  [p2p-dist bounds]
  ;NOTE: the size of the matrix is kept equal to p2p-dist matrix in order
  ; to get the right index for the coupling sequence with the limits passed
  (let [[rows columns] (shape p2p-dist)
        CA             (new-matrix :vectorz rows columns)]
    (for [[i-start j-start i-end j-end :as limit] bounds]
      (do (compute-CA! CA p2p-dist i-start j-start i-end j-end); mutates CA
          {:dist (mget CA i-end j-end) :couple (find-sequence CA limit)}))))

(defn cartesian
  "computes the cartesian product of two or more sequences. If only one sequence
  is provided then a sequence of each element wrapped on a list is returned.
  Note that the cartesian product of an empty sequence is a sequence with an
  emtpy list, since there is one way to take the cartesian product of no lists"
  ([x-coll]
   (if (empty? x-coll)
    '(())
     (map list x-coll)))
  ([x-coll & colls]
   (for [x    x-coll
         y    (apply cartesian colls)]
      (cons x y))))
