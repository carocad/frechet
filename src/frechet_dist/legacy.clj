(ns frechet-dist.shared
  (:require [clojure.core.matrix :as matrix :refer [get-row row-count mget mset!
                                                    compute-matrix shape immutable]]))

(defn- bounds
  "compute the get-bounds of a matrix (mtx) taking into account that mget is
  zero indexed"
  [mtx] (apply conj [0 0] (mapv dec (shape mtx))))


(defn- compute-columns
  [P Q dist-fn row_i]
  (let [P-row (get-row P row_i)]
    (for [j (range (row-count Q))]
      (dist-fn P-row (get-row Q j)))))

(defn- compute-rows
  [P Q dist-fn [row-start row-end]]
  (for [i (range row-start row-end)]
    (compute-columns P Q dist-fn i)))

(defn concurrent-distance
  "computes the distance between all the possible point combinations of the two
  curves P and Q using the dist-fn"
  [P Q dist-fn]
  (let [n-cores  (.availableProcessors (Runtime/getRuntime))
        n-rows   (row-count P)]
    (if (> n-cores n-rows)
      (compute-matrix :vectorz [n-rows (row-count Q)] (fn [i j] (dist-fn (get-row P i) (get-row Q j))))
      (let [starts   (map int (range 0 (inc n-rows) (/ n-rows n-cores)))
            bounds   (map vector starts (rest starts))
            rows     (apply concat (pmap (partial compute-rows P Q dist-fn) bounds))]
        (matrix/matrix :vectorz rows)))))

(defn link-matrix
  "calculate the frechet distance among all possible discrete parametrization
  of the curves P and Q using dist-fn. The discrete frechet distance is
  returned along the coupling sequence."
  ([p2p-dist]
   (link-matrix p2p-dist (bounds p2p-dist)))
  ([p2p-dist [i-start j-start i-end j-end]]
   ;NOTE: the size of the matrix is kept equal to p2p-dist matrix in order
  ; to get the right index for the coupling sequence when the limits passed
  (let [CA         (compute-matrix :vectorz (shape p2p-dist) (fn [i j] -1))
                                            ;shape is 1-indexed but mget is zero-indexed
        cd-fn      (fn cd [i j] ;cd : calculate distance
                     (cond
                      (> (mget CA i j) -1) :default ; do nothing
                      (and (= i i-start) (= j j-start)) (mset! CA i j (mget p2p-dist i-start j-start))
                      (and (> i i-start) (= j j-start)) (mset! CA i j (max (cd (dec i) j-start) (mget p2p-dist i j-start)))
                      (and (= i i-start) (> j j-start)) (mset! CA i j (max (cd i-start (dec j)) (mget p2p-dist i-start j)))
                      (and (> i i-start) (> j j-start)) (mset! CA i j (max (min (cd (dec i) j)
                                                                                (cd (dec i) (dec j))
                                                                                (cd i (dec j)))
                                                                           (mget p2p-dist i j))))
                     (mget CA i j))
        dist       (cd-fn i-end j-end)]
    {:dist dist :CA (immutable CA)})))

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
       (cond
         (and (= i i-start) (= j j-start)) (reverse (persistent! (conj! path [i-start j-start])))
         (and (> i i-start) (= j j-start)) (recur (dec i) j (conj! path [i j]))
         (and (= i i-start) (> j j-start)) (recur i (dec j) (conj! path [i j]))
         (and (> i i-start) (> j j-start))
           (let [diag (mget CA (dec i) (dec j))
                 left (mget CA (dec i) j)
                 top  (mget CA i (dec j))]
           (cond
             (and (>= left diag) (>= top diag)) (recur (dec i) (dec j) (conj! path [i j]))
             (and (>= diag left) (>= top left)) (recur (dec i) j (conj! path [i j]))
             (and (>= diag top) (>= left top)) (recur i (dec j) (conj! path [i j])))))))); return value

