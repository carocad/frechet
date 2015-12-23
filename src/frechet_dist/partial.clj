(ns frechet-dist.partial
  (:require [clojure.core.matrix :refer [shape]]
            [frechet-dist.shared :refer [link-matrix]]))

(defn- invalid-bounds?
  "check if the current bounds are valid. Valid bounds are such that the curve
  is reduced to a minimum of two points"
  [[is js ie je]]
  (if (or (> (- ie is) 1) (> (- je js) 1))
    true
    false))

(defn- delimiter
  "create a function which will return the bounding parameters with which
  the coupling sequence and frechet distance of two curves P and Q will
  be calculated."
  [dir]
  (condp = dir  ;start  end
    :right  (fn [[is js ie je]] [is       js       ie    (dec je)])
    :left   (fn [[is js ie je]] [is       (inc js) ie         je])
    :bottom (fn [[is js ie je]] [is       js       (dec ie)   je])
    :top    (fn [[is js ie je]] [(inc is) js       ie         je])))

(defn- opt-bounds
  "optimize the boundaries of CA according to the move strategy such that the
  frechet distance is minimize"
  [p2p-dist move limits]
  (loop [curr-bounds    limits
         curr-leash     (first (link-matrix p2p-dist curr-bounds))]
    (let [new-bounds    (move curr-bounds)
          new-leash     (first (link-matrix p2p-dist new-bounds))]
      (if (and (not (invalid-bounds? curr-bounds)) (> curr-leash new-leash))
        (recur new-bounds new-leash)
        curr-bounds))))

(defn relax-boundaries
  "iteratively find new boundaries for CA such that the frechet distance is
  lower inside this boundaries than if they were included"
  [p2p-dist]
  (loop [curr-bounds     (apply conj [0 0] (shape p2p-dist))]
    ;the boundary optimizations become more expensive as the number of samples increase
    (let [bound-right    (future (opt-bounds p2p-dist (delimiter :right) curr-bounds))
          bound-left     (future (opt-bounds p2p-dist (delimiter :left) curr-bounds))
          bound-top      (future (opt-bounds p2p-dist (delimiter :top) curr-bounds))
          bound-bottom   (future (opt-bounds p2p-dist (delimiter :bottom) curr-bounds))
          new-bounds     [(first @bound-top)    (second @bound-left)
                          (nth @bound-bottom 2) (nth @bound-right 3)]]
      (if (not= curr-bounds new-bounds)
        (recur new-bounds)
        curr-bounds))))
