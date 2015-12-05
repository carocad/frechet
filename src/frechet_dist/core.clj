(ns frechet-dist.core
  (:refer-clojure :exclude [* - + /  == <= >= not= ]) ;max has a bug. Waiting for fix in core.matrix
  (:require [clojure.math.numeric-tower :as math]
            [clojure.core.matrix :refer :all]
            [clojure.core.matrix.operators :refer :all :exclude [min max]]))

(defn- euclidean-dist
  "Calculate the euclidean distance of two points in space.
  This function works on any 1xN dimmensional vectors"
  [u v]
  (let [pow2 (fn [x] (math/expt x 2))]
    (->> (map (comp pow2 -) u v)
         (reduce +)
         (math/sqrt))))

(defn frechet-dist
  "calculate the discrete frechet distance between two curves"
  ([P Q]
   (frechet-dist P Q euclidean-dist))
  ([P Q dist-fn]
  (let [length-P   (first (shape P))
        length-Q   (first (shape Q))
        CA         (matrix :vectorz (for [i (range length-P)]
                                      (for [j (range length-Q)]
                                        (dist-fn (get-row P i) (get-row Q j)))))
        coupling   (loop [i (- length-P 1)
                          j (- length-Q 1)
                          path (transient [])]
                     (cond
                      (and (= i 1) (= j 1)) (reverse (persistent! (conj! path [1 1]))) ; return value
                      (and (> i 1) (= j 1)) (recur (dec i) j (conj! path [i j]))
                      (and (= i 1) (> j 1)) (recur i (dec j) (conj! path [i j]))
                      (and (> i 1) (> j 1))
                          (condp = (apply min [(mget CA (dec i) (dec j))
                                               (mget CA (dec i) j)
                                               (mget CA i (dec j))])
                            (mget CA (dec i) (dec j)) (recur (dec i) (dec j) (conj! path [i j]))
                            (mget CA (dec i) j) (recur (dec i) j (conj! path [i j]))
                            (mget CA i (dec j)) (recur i (dec j) (conj! path [i j]))
                            (println "Wrong matrix iteration i=" i " j=" j))))]
    [(apply max (map #(mget CA (first %) (second %)) coupling)) ; frechet distance
     coupling])))
