(ns frechet-dist.partial
  (:require [clojure.core.matrix :refer [shape get-column get-row]]
            [frechet-dist.shared :refer [link-matrix bounds]]))

(defn valid-bounds?
  "check if the current bounds are valid. Valid bounds are such that the curve
  is reduced to a minimum of two points (with zero indexed bounds)"
  [[is js ie je]]
  (and (>= (- ie is) 1) (>= (- je js) 1)))

(defn- min-indexed
  "returns the a vector [index value] where index is the position at which the
  minimum value occurs in data."
  [data]
  (->> (map-indexed vector data)
       (apply min-key second)))

(defn- nearest-point
  "find which is the nearest point to i or j based on the distances given by
  p2p-dist."
  [p2p-dist & {:keys [i j]}]
  (cond
   (nil? i) (first (min-indexed (get-column p2p-dist j)))
   (nil? j) (first (min-indexed (get-row p2p-dist i)))))

(defn find-boundaries
  "find all nearest neighbours of the start and end points of each curve,
  including the default start and end of each curve. Only distinct start/end
  points are returned."
  [p2p-dist]
  (let [sdef   [0, 0]
        si     [0 (nearest-point p2p-dist :i 0)]
        sj     [(nearest-point p2p-dist :j 0) 0]
        edef   (subvec (bounds p2p-dist) 2)
        ei     [(first edef) (nearest-point p2p-dist :i (first edef))]
        ej     [(nearest-point p2p-dist :j (second edef)) (second edef)]]
    [(distinct [sdef si sj]) (distinct [edef ei ej])]))

(defn product
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
         y    (apply product colls)]
      (cons x y))))
