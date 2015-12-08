(ns frechet-dist.core
  (:refer-clojure :exclude [* - + /  == <= >= not= ]) ;max has a bug. Waiting for fix in core.matrix
  (:require [clojure.math.numeric-tower :as math]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all :exclude [min max]]))

(defn- over-threshold?
  [dist epsilon]
  (if (> (/ dist 2) epsilon) dist nil))

(defn- re-sample
  "re-sample the line segment given by pi pj as many times as necessary
  to ensure a distance lower than twice epsilon between two points."
  [pi pj p2p-dist epsilon]
  (let [n-times  (math/ceil (/ p2p-dist epsilon)) ; number of subintervals to insert
        deltap   (/ (- pj pi) n-times)] ; size of each interval
    (for [a (range n-times)]
      (+ pi (* a deltap)))))

(defn refine
  "refine curve P so that the distance between any two consecutive points is
  not greater than twice epsilon. This function is useful for getting better
  approximations of the Frechet distance via the discrete frechet distance,i.e.
  the discrete frechet distance converge to the frechet distance for better
  sampled curves"
  ([P epsilon]
   (refine P epsilon distance))
  ([P epsilon dist-fn]
   (matrix :vectorz
    (loop [i      0
           output (transient [])]
      (let [j     (inc i)
            Pi    (get-row P i)
            Pj    (get-row P j)]
        (if (= j (row-count P))
          (persistent! (conj! output Pi))
          (if-let [p2p-dist (over-threshold? (dist-fn Pi Pj) epsilon)]
            (recur (inc i) (reduce #(conj! %1 %2) output (re-sample Pi Pj p2p-dist epsilon)))
            (recur (inc i) (conj! output Pi)))))))))


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
  (let [CA        (matrix :vectorz (for [i (range (row-count P))]
                                      (for [j (range (row-count Q))]
                                        (dist-fn (get-row P i) (get-row Q j)))))
        coupling  (loop [i (dec (row-count P))
                         j (dec (row-count Q))
                          path (transient [])]
                     (cond
                      (and (= i 0) (= j 0)) (reverse (persistent! (conj! path [0 0]))) ; return value
                      (and (> i 0) (= j 0)) (recur (dec i) j (conj! path [i j]))
                      (and (= i 0) (> j 0)) (recur i (dec j) (conj! path [i j]))
                      (and (> i 0) (> j 0))
                          (condp = (apply min [(mget CA (dec i) (dec j))
                                               (mget CA (dec i) j)
                                               (mget CA i (dec j))])
                            (mget CA (dec i) (dec j)) (recur (dec i) (dec j) (conj! path [i j]))
                            (mget CA (dec i) j) (recur (dec i) j (conj! path [i j]))
                            (mget CA i (dec j)) (recur i (dec j) (conj! path [i j])))))]
    [(apply max (map #(mget CA (first %) (second %)) coupling)) ; frechet distance
     coupling])))
