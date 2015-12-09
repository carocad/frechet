(ns frechet-dist.core
  (:require [clojure.core.matrix :refer [matrix row-count mget distance emax pm]]
            [frechet-dist.utils :refer [delimiter max-leash opt-bounds]]
            [frechet-dist.shared :refer [dist-matrix find-sequence]] :reload))

(defn partial-frechet-dist
  ([P Q]
   (partial-frechet-dist P Q distance))
  ([P Q dist-fn]
   (let [CA        (dist-matrix P Q dist-fn)
         size      [0 0 (dec (row-count P)) (dec (row-count Q))]
         ;_              (pm CA)
         limits    (->> (opt-bounds CA (delimiter :right) size)
                        (opt-bounds CA (delimiter :left))
                        (opt-bounds CA (delimiter :top))
                        (opt-bounds CA (delimiter :bottom)))
;NOTE This iteration could be done several times until no change happens
;currently the bound changes don't reflect back on the previous bound optimizations
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
