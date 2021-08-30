(ns carocad.frechet.shared
  "Common functions used by both frechet and partial frechet distance.
  Internal implementations; subject to breaking changes without warning!")

(defn get2D
  [^objects array2D [i j]]
  (let [row (aget array2D i)]
    (aget ^doubles row j)))

(defn row ^doubles [^objects array-2D i] (aget array-2D i))

(defn column
  [^objects array-2D j]
  (let [row-count (dec (alength array-2D))
        column    (double-array (alength (row array-2D 0)))]
    (dotimes [i row-count]
      (aset column i ^double (get2D array-2D [i j])))
    column))

(defn point-distance
  "computes the distance between all the possible point combinations of the two
  curves P and Q using the dist-fn"
  ^objects [P Q dist-fn]
  (let [row-count    (count P)
        column-count (count Q)
        result       (object-array row-count)]
    (dotimes [i row-count]
      (let [current-row (double-array column-count)]
        (aset result i current-row)
        (dotimes [j column-count]
          (let [value (dist-fn (get P i) (get Q j))]
            (aset current-row j ^double value)))))
    result))

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
  ^objects
  [P Q dist-fn]
  (let [row-count    (count P)
        column-count (count Q)
        result       (object-array row-count)]
    (dotimes [i row-count]
      (let [current-row  (double-array column-count)
            _            (aset result i current-row)        ;; set it here to avoid edge case on [0 0]
            previous-row ^doubles (aget result (max 0 (dec i)))]
        (dotimes [j column-count]
          (let [previous-j (max 0 (dec j))
                value      (max (min (aget previous-row previous-j) ;; diagonal
                                     (aget previous-row j)  ;; above
                                     ;; current-row contains only 0 after creation
                                     (if (= j previous-j) ##Inf (aget current-row previous-j))) ;; behind
                                (dist-fn (get P i)
                                         (get Q j)))]
            (aset current-row j ^double value)))))
    result))

(defn find-sequence
  "Given a link-distance 2D array (of doubles) CA finds the path enclosed by the limits
  i-start j-start i-end j-end that minimizes the distance between the two
  curves from which CA was created."
  [^objects CA]
  (let [max-i (dec (alength CA))
        max-j (dec (alength ^doubles (aget CA max-i)))]
    (loop [[i j :as index] [max-i max-j]
           path (list)]
      (let [prev-i (max 0 (dec i))
            prev-j (max 0 (dec j))]
        (if (and (= i 0) (= j 0))                           ;; reached the start. Return
          (conj path index)
          (let [candidates (list [prev-i j]                 ;; behind
                                 [i prev-j]                 ;; above
                                 [prev-i prev-j])           ;; diagonal
                previous   (apply min-key
                                  #(get2D CA %)
                                  (remove #(= index %) candidates))]

            (recur previous (conj path index))))))))
