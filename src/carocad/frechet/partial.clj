(ns carocad.frechet.partial
  (:require [carocad.frechet.shared :as common]))

(defn valid-bounds?
  "check if the current bounds are valid. Valid bounds are such that
   the curve contains at least 2 points"
  [[is js ie je]]
  (and (>= (- ie is) 1) (>= (- je js) 1)))

(defn- min-index
  "returns a vector [index value] where index is the position at which the
  minimum value occurs in numbers"
  [numbers]
  (first (apply min-key second (map-indexed vector numbers))))

(defn find-boundaries
  "Given a point to point distance matrix (p2p-dist) obtained from
   2 curves P and Q find the nearest point to P's start/end in Q (resp. from Q in P)"
  [p2p-dist]
  (let [def-start [0 0]
        si        [0 (min-index (common/row p2p-dist 0))]   ;; closest point in Q to P-start
        sj        [(min-index (common/column p2p-dist 0)) 0] ;; closes point in P to Q-start
        [i_n j_n :as def-end] (subvec (common/bounds p2p-dist) 2)
        ei        [i_n (min-index (common/row p2p-dist i_n))] ;; closest point in Q to P-end
        ej        [(min-index (common/column p2p-dist j_n)) j_n]] ;; closest point in P to Q-end
    [(distinct [def-start si sj])                           ;; starts
     (distinct [def-end ej ei])]))                          ;; ends

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
   (for [x x-coll
         y (apply cartesian colls)]
     (cons x y))))

(defn part-curve-dist
  "calculate the link distance and the coupling sequence of all possible
  boundaries (all-bounds). Returns all the partial frechet distances and their
  couplings"
  [P Q dist-fn all-bounds]
  (for [[min-i min-j max-i max-j] all-bounds]
    (let [P           (subvec P min-i (inc max-i))
          Q           (subvec Q min-j (inc max-j))
          link-matrix (common/link-matrix P Q dist-fn)
          total       (common/get2D link-matrix [(- max-i min-i) (- max-j min-j)])
          coupling    (common/find-sequence link-matrix)]
      {:carocad.frechet/distance total
       :carocad.frechet/couple   (for [[i j] coupling]
                                   [(+ min-i i) (+ min-j j)])})))
