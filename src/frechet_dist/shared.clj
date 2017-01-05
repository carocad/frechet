(ns frechet-dist.shared
  (:require [clojure.core.matrix :as matrix
             :refer [get-row row-count mget mset!]]))
            ;[taoensso.timbre.profiling :refer [defnp p]]))

(defn bound-zero
  "compute the get-bounds of a matrix (mtx) taking into account that mget is
  zero indexed"
  [mtx] (concat [0 0] (map dec (matrix/shape mtx))))

;; TODO: this is UGLY as hell but right now is the best option to move fast
;;       I should refactor this to provide a more elegant solution
(defn point-distance
  "computes the distance between all the possible point combinations of the two
  curves P and Q using the dist-fn"
  [P Q dist-fn]
  (if (and (matrix/numerical? P) (matrix/numerical? Q))
    (let [P2 (matrix/coerce :vectorz P)
          Q2 (matrix/coerce :vectorz Q)]
      (matrix/compute-matrix :vectorz [(row-count P2) (row-count Q2)]
                  (fn [ i j] (dist-fn (get-row P2 i) (get-row Q2 j)))))
    (matrix/compute-matrix :vectorz [(count P) (count Q)]
                (fn [ i j] (dist-fn (get P i) (get Q j))))))

(defn compute-CA!
  "mutates CA between the boundaries specified by i,j to calculate the link
  distance"
  [CA p2p-dist i-start j-start i-end j-end]
  (dorun ;; TODO: transform this into a double reduce form with an strategy similar
         ;;       to the one used in the levenshtein distance
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
       (mset! CA i j value))))

(defn link-matrix
  "calculate the link distance among all possible discrete parametrization
  of the curves P and Q using dist-fn. The discrete frechet distance is
  returned along the coupling sequence."
  ([p2p-dist]
   (link-matrix p2p-dist (bound-zero p2p-dist)))
  ([p2p-dist limits]
   ;NOTE: the size of the matrix is kept equal to p2p-dist matrix in order
  ; to get the right index for the coupling sequence with the limits passed
   (let [[i-start j-start i-end j-end] limits
         [rows columns]                (matrix/shape p2p-dist)
         CA                            (matrix/new-matrix :vectorz rows columns)]
     (compute-CA! CA p2p-dist i-start j-start i-end j-end)
    {:dist (mget CA i-end j-end) :CA CA})))

(defn find-sequence
  "Given a link-distance matrix CA find the path enclosed by the limits
  i-start j-start i-end j-end that minimizes the distance between the two
  curves from which CA was created."
  ([CA]
   (find-sequence CA (bound-zero CA)))
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
