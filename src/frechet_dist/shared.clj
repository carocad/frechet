(ns frechet-dist.shared
  (:require [clojure.core.matrix :refer [matrix get-row row-count mget emax mset! compute-matrix]]))

(defn max-leash
  "base on the distance matrix CA find the maximum distance using the provided
  coupling sequence"
  [CA coupling]
  (emax (map #(apply mget CA %) coupling)))

(defn find-sequence
  "Given a point2point distance matrix CA find the path enclosed by the limits
  i-start j-start i-end j-end that minimizes the distance between the two
  curves from which CA was created."
  [CA [i-start j-start i-end j-end]]
  (loop [i i-end
         j j-end
         path (transient [])]
    (cond
     (and (= i i-start) (= j j-start)) (reverse (persistent! (conj! path [i-start j-start]))) ; return value
     (and (> i i-start) (= j j-start)) (recur (dec i) j (conj! path [i j]))
     (and (= i i-start) (> j j-start)) (recur i (dec j) (conj! path [i j]))
     (and (> i i-start) (> j j-start))
       (let [diag (mget CA (dec i) (dec j))
             left (mget CA (dec i) j)
             top  (mget CA i (dec j))]
         (cond
          (and (>= left diag) (>= top diag)) (recur (dec i) (dec j) (conj! path [i j]))
          (and (>= diag left) (>= top left)) (recur (dec i) j (conj! path [i j]))
          (and (>= diag top) (>= left top)) (recur i (dec j) (conj! path [i j])))))))

(defn dist-matrix
  "calculate the frechet distance among all possible discrete parametrization
  of the curves P and Q using dist-fn."
  [P Q dist-fn]
  (let [length-P   (row-count P)
        length-Q   (row-count Q)
        CA         (compute-matrix :vectorz [length-P length-Q] (fn [i j] -1))
        p2p-dist   (compute-matrix :vectorz [length-P length-Q]
                                   (fn [i j] (dist-fn (get-row P i)
                                                      (get-row Q j))))
        cd-fn      (fn cd [i j] ;cd : calculate distance
                     (cond
                      (> (mget CA i j) -1) :default ; do nothing
                      (and (= i 0) (= j 0)) (mset! CA i j (mget p2p-dist 0 0))
                      (and (> i 0) (= j 0)) (mset! CA i j (max (cd (dec i) 0) (mget p2p-dist i 0)))
                      (and (= i 0) (> j 0)) (mset! CA i j (max (cd 0 (dec j)) (mget p2p-dist 0 j)))
                      (and (> i 0) (> j 0)) (mset! CA i j (max (min (cd (dec i) j)
                                                                    (cd (dec i) (dec j))
                                                                    (cd i (dec j)))
                                                               (mget p2p-dist i j))))
                     (mget CA i j))
        dist       (cd-fn (dec length-P) (dec length-Q))]
    [dist CA]))
