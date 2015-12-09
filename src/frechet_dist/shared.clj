(ns frechet-dist.shared
  (:require [clojure.core.matrix :refer [matrix get-row row-count mget]]))

(defn dist-matrix
  "calculate the point to point distance among all possible combinations
  of P and Q elements. The distance is calculated using dist-fn"
  [P Q dist-fn]
  (matrix :vectorz (for [i (range (row-count P))]
                     (for [j (range (row-count Q))]
                       (dist-fn (get-row P i) (get-row Q j))))))

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
       (condp = (min (mget CA (dec i) (dec j))
                     (mget CA (dec i) j)
                     (mget CA i (dec j)))
         (mget CA (dec i) (dec j)) (recur (dec i) (dec j) (conj! path [i j]))
         (mget CA (dec i) j)       (recur (dec i) j (conj! path [i j]))
         (mget CA i (dec j))       (recur i (dec j) (conj! path [i j]))))))
