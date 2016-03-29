(ns frechet-dist.shared
  (:import [mikera.matrixx AMatrix])
  (:require [clojure.core.matrix :as matrix
             :refer [get-row row-count mget mset! compute-matrix shape]]))
            ;[taoensso.timbre.profiling :refer [defnp p]]))

(defprotocol Matrizable
  (wrap [coll] "convert a Clojure collection to a vectorz representation"))

(extend-type AMatrix
  Matrizable
  (wrap [coll] coll)) ; already a matrix; nothing to do

(extend-type clojure.lang.Sequential
  Matrizable
  (wrap [coll] (matrix/matrix :vectorz coll))) ; convert the collection to a matrix

(defn bounds
  "compute the get-bounds of a matrix (mtx) taking into account that mget is
  zero indexed"
  [mtx] (concat [0 0] (map dec (shape mtx))))

(defn point-distance
  "computes the distance between all the possible point combinations of the two
  curves P and Q using the dist-fn"
  [P Q dist-fn]
  (compute-matrix :vectorz [(row-count P) (row-count Q)]
                  (fn [ i j] (dist-fn (get-row P i) (get-row Q j)))))


(defn find-sequence
  "Given a point2point distance matrix CA find the path enclosed by the limits
  i-start j-start i-end j-end that minimizes the distance between the two
  curves from which CA was created."
  ([CA]
   (find-sequence CA (bounds CA)))
  ([CA [i-start j-start i-end j-end]]
   (loop [i i-end
          j j-end
          path (transient [])]
     (let [prev-i (dec i)
           prev-j (dec j)]
       (cond
         (and (> i i-start) (> j j-start))
         (let [diag (mget CA prev-i prev-j)
               left (mget CA prev-i j)
               top  (mget CA i prev-j)]
           (cond
             (and (>= left diag) (>= top diag)) (recur prev-i prev-j (conj! path [i j]))
             (and (>= diag left) (>= top left)) (recur prev-i j (conj! path [i j]))
             (and (>= diag top) (>= left top)) (recur i prev-j (conj! path [i j]))))
         (and (> i i-start) (= j j-start)) (recur prev-i j (conj! path [i j]))
         (and (= i i-start) (> j j-start)) (recur i prev-j (conj! path [i j]))
         (and (= i i-start) (= j j-start)) (reverse (persistent! (conj! path [i-start j-start]))))))))

(defn link-matrix
  "calculate the frechet distance among all possible discrete parametrization
  of the curves P and Q using dist-fn. The discrete frechet distance is
  returned along the coupling sequence."
  ([p2p-dist]
   (link-matrix p2p-dist (bounds p2p-dist)))
  ([p2p-dist [i-start j-start i-end j-end]]
   ;NOTE: the size of the matrix is kept equal to p2p-dist matrix in order
  ; to get the right index for the coupling sequence when the limits passed
   (let [[rows columns] (shape p2p-dist)
         CA             (matrix/new-matrix :vectorz rows columns)]
     (dorun
     (for [i (range i-start (inc i-end))
           j (range j-start (inc j-end))
      :let [prev-i (dec i)
            prev-j (dec j)
            value (cond
         (and (> i i-start) (> j j-start)) (max (mget p2p-dist i j)
                                                (min (mget CA prev-i j)
                                                     (mget CA prev-i prev-j)
                                                     (mget CA i prev-j)))
         (and (> i i-start) (= j j-start)) (max (mget p2p-dist i j)
                                                (mget CA prev-i j))
         (and (= i i-start) (> j j-start)) (max (mget p2p-dist i-start j)
                                                (mget CA i prev-j))
         (and (= i i-start) (= j j-start)) (mget p2p-dist i j))]]
       (mset! CA i j value)))
    {:dist (mget CA i-end j-end) :CA CA})))
