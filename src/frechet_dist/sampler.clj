(ns frechet-dist.sampler
  (:require [clojure.core.matrix :refer [get-row row-count distance sub add mul div]]))
            ;[taoensso.timbre.profiling :refer [defnp]]))

(defn- interpolate
  "returns an interpolation between the points pi and pj rounding the
  ratio using the round function. round defaults to ceil. The results of rounding
  the ratio is the size of the returned sequence"
  ([pi pj ratio]
   (interpolate pi pj ratio #(Math/ceil %)))
  ([pi pj ratio round]
   (let [n-times  (round ratio) ; number of subintervals to insert
         deltap   (div (sub pj pi) n-times)] ; size of each interval
    (for [a (range n-times)]
      (add pi (mul a deltap))))))

(defn refine
  "refine curve P so that the distance between any two consecutive points is
  not greater than epsilon. This function is useful for getting better
  approximations of the Frechet distance via the discrete frechet distance,i.e.
  the discrete frechet distance converge to the frechet distance for better
  sampled curves"
  ([P epsilon]
   (refine P epsilon distance))
  ([P epsilon dist-fn]
   (let [pij-dist   (map dist-fn P (rest P))
         sampler    (fn [index dist]
                      (if (> dist epsilon)
                        (interpolate (get-row P index) (get-row P (inc index)) (/ dist epsilon))
                        (list (get-row P index))))
         sampled-P  (map-indexed sampler pij-dist)]
     (conj (into [] (apply concat sampled-P))
           (peek P)))))
