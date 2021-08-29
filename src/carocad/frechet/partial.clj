(ns carocad.frechet.partial
  (:require [clojure.core.matrix :as matrix]
            [carocad.frechet.shared :as common]))

(defn valid-bounds?
  "check if the current bounds are valid. Valid bounds are such that the curve
  can be reduced to a minimum of two points (with zero indexed bounds)"
  [[is js ie je]]
  (and (>= (- ie is) 1) (>= (- je js) 1)))

(defn- min-indexed
  "returns a vector [index value] where index is the position at which the
  minimum value occurs in data."
  [data]
  (apply min-key second (map-indexed vector data)))

(defn- nearest-point
  "find which is the nearest point to i or j based on the distances given by
  p2p-dist."
  [p2p-dist i j]
  (cond
   (nil? i) (first (min-indexed (matrix/get-column p2p-dist j)))
   (nil? j) (first (min-indexed (matrix/get-row p2p-dist i)))))

(defn find-boundaries
  "Given a point to point distance matrix (p2p-dist) obtained from
   2 curves P and Q find the nearest point to P's start/end in Q (resp. from Q in P)"
  [p2p-dist]
  (let [def-start             [0 0]
        si                    [0 (nearest-point p2p-dist 0 nil)]
        sj                    [(nearest-point p2p-dist nil 0) 0]
        [i_n j_n :as def-end] (subvec (vec (common/bound-zero p2p-dist)) 2)
        ei                    [i_n (nearest-point p2p-dist i_n nil)]
        ej                    [(nearest-point p2p-dist nil j_n) j_n]]
    [(distinct [def-start si sj]) ;; starts
     (distinct [def-end ei ej])])) ;; ends

(defn part-curve-dist
  "calculate the link distance and the coupling sequence of all possible
  boundaries (bounds). Returns all the partial frechet distances and their
  couplings"
  [p2p-dist bounds]
  ;NOTE: the size of the matrix is kept equal to p2p-dist matrix in order
  ; to get the right index for the coupling sequence with the limits passed
  (let [[rows columns] (matrix/shape p2p-dist)
        CA             (matrix/new-matrix :vectorz rows columns)]
    (for [[i-start j-start i-end j-end :as limit] bounds]
      (do (common/compute-CA! CA p2p-dist i-start j-start i-end j-end); mutates CA
          {:dist (matrix/mget CA i-end j-end) :couple (common/find-sequence CA limit)}))))

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
