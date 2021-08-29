(ns carocad.frechet.shared
  (:require [clojure.core.matrix :as matrix]))

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
      (matrix/compute-matrix :vectorz [(matrix/row-count P2) (matrix/row-count Q2)]
                  (fn [i j] (dist-fn (matrix/get-row P2 i) (matrix/get-row Q2 j)))))
    (matrix/compute-matrix :vectorz [(count P) (count Q)]
                (fn [i j] (dist-fn (nth P i) (nth Q j))))))

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
                        (and (> i i-start) (> j j-start)) (max (matrix/mget p2p-dist i j)
                                                               (min (matrix/mget CA prev-i j)
                                                                    (matrix/mget CA prev-i prev-j)
                                                                    (matrix/mget CA i prev-j)))
                        (and (> i i-start) (= j j-start)) (max (matrix/mget p2p-dist i j)
                                                               (matrix/mget CA prev-i j))
                        (and (= i i-start) (> j j-start)) (max (matrix/mget p2p-dist i-start j)
                                                               (matrix/mget CA i prev-j))
                        (and (= i i-start) (= j j-start)) (matrix/mget p2p-dist i j))]]
       (matrix/mset! CA i j value))))

#_(defn link-matrix
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
      {:dist (matrix/mget CA i-end j-end) :CA CA})))

#_(defn find-sequence
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
           (let [diag (matrix/mget CA prev-i prev-j)
                 left (matrix/mget CA prev-i j)
                 top  (matrix/mget CA i prev-j)]
             (cond
               (and (>= left diag) (>= top diag))
               (recur prev-i prev-j (conj! path [i j]))

               (and (>= diag left) (>= top left))
               (recur prev-i j (conj! path [i j]))

               (and (>= diag top) (>= left top))
               (recur i prev-j (conj! path [i j]))))

           ;; reached the leftmost column, can only go up
           (and (> i i-start) (= j j-start))
           (recur prev-i j (conj! path [i j]))

           ;; reached the topmost row, can only go left
           (and (= i i-start) (> j j-start))
           (recur i prev-j (conj! path [i j]))

           ;; reached the start. Return
           (and (= i i-start) (= j j-start))
           (reverse (persistent! (conj! path [i-start j-start]))))))))

(defn bounds
  "returns a vector of [min-i min-j max-i max-j] where i and j are
   the row and column indexes on the 2D array"
  [^objects array-2d]
  (let [max-i (dec (alength array-2d))
        max-j (dec (alength ^doubles (aget array-2d max-i)))]
    [0 0 max-i max-j]))

(defn link-matrix
  "Given 2 curves P an Q compute the link distance between them using dist-fn.
  Returns a 2D array of doubles."
  ^objects [P Q dist-fn]
  (let [row-count    (count P)
        column-count (count Q)
        result       (object-array row-count)]
    (dotimes [i row-count]
      (let [current-row  (double-array column-count)
            _            (aset result i current-row)
            previous-row (aget result (max 0 (dec i)))]
        (dotimes [j column-count]
          (let [value (max (min (aget ^doubles previous-row (max 0 (dec j)))     ;; diagonal
                                (aget ^doubles previous-row j)                   ;; above
                                (aget current-row (max 0 (dec j))))     ;; behind
                           (dist-fn (get P i)
                                    (get Q j)))]
            (aset current-row j ^double value)))))
    result))

(defn get2D
  [CA [i j]]
  (let [row (aget ^objects CA i)]
    (aget ^doubles row j)))

(defn find-sequence2
  "Given a link-distance 2D array (of doubles) CA finds the path enclosed by the limits
  i-start j-start i-end j-end that minimizes the distance between the two
  curves from which CA was created."
  [CA [i-start j-start i-end j-end]]
  (loop [[i j :as index] [i-end j-end]
         path (list)]
    (let [prev-i (max i-start (dec i))
          prev-j (max j-start (dec j))]
      (if (and (= i i-start) (= j j-start)) ;; reached the start. Return
        (conj path index)
        (let [previous (min-key #(get2D CA %)
                                [prev-i j] ;; behind
                                [i prev-j] ;; above
                                [prev-i prev-j])] ;; diagonal
          (recur previous (conj path index)))))))
