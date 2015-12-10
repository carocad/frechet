(ns frechet-dist.core
  (:require [clojure.core.matrix :refer [matrix row-count mget distance]]
            [frechet-dist.partial :refer [relax-boundaries]]
            [frechet-dist.shared :refer [max-leash dist-matrix find-sequence]] :reload))

(defn partial-frechet-dist
  "compute the partial frechet distance among P and Q. The partial distance is
  calculated as the frechet distance among R and T, where R and T are R and T
  are the longest continous subcurves from P and Q that minimize the frechet
  distance."
  ([P Q]
   (partial-frechet-dist P Q distance))
  ([P Q dist-fn]
   (let [CA        (dist-matrix P Q dist-fn)
         limits    (relax-boundaries CA)
         coupling  (find-sequence CA limits)
         min-leash (max-leash CA coupling)]
     [min-leash coupling])))

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
  (let [CA        (dist-matrix P Q dist-fn)
        coupling  (find-sequence CA [0 0 (dec (row-count P)) (dec (row-count Q))])
        leash     (max-leash CA coupling)]
    [leash coupling])))
