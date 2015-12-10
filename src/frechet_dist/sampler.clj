(ns frechet-dist.sampler
  (:refer-clojure :exclude [- + *])
  (:require [clojure.math.numeric-tower :as math]
            [clojure.core.matrix :refer [matrix get-row row-count distance]]
            [clojure.core.matrix.operators :refer [- + *]]))

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
