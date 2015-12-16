(ns frechet-dist.partial
  (:require [clojure.core.matrix :refer [shape]]
            [frechet-dist.shared :refer [max-leash find-sequence dist-matrix]]))

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
  [CA move limits]
  (loop [curr-bounds    limits
         curr-leash     (max-leash CA (find-sequence CA curr-bounds))]
    (let [new-bounds    (move curr-bounds)
          new-leash     (max-leash CA (find-sequence CA new-bounds))]
      (if (> curr-leash new-leash)
        (recur new-bounds new-leash)
        curr-bounds))))

(defn relax-boundaries
  "iteratively find new boundaries for CA such that the frechet distance is
  lower inside this boundaries than if they were included"
  [CA]
  (loop [curr-bounds     [0 0 (dec (first (shape CA))) (dec (second (shape CA)))]]
    ;the boundary optimizations become more expensive as the number of samples increase
    (let [bound-right    (future (opt-bounds CA (delimiter :right) curr-bounds))
          bound-left     (future (opt-bounds CA (delimiter :left) curr-bounds))
          bound-top      (future (opt-bounds CA (delimiter :top) curr-bounds))
          bound-bottom   (future (opt-bounds CA (delimiter :bottom) curr-bounds))
          new-bounds     [(first @bound-top)    (second @bound-left)
                          (nth @bound-bottom 2) (nth @bound-right 3)]]
      (if (not= curr-bounds new-bounds)
        (recur new-bounds)
        curr-bounds))))

;BUG currently this way of finding the minimum frechet distance is wrong as it
; is doesn't take into account the bug fix of the distance matrix CA !!
; Instead the frechet distance MUST be calculated at every iteration, this can
; preferably be done using a simple point to point distance matrix and creating
; new CA matrices such that the frechet distance can be calculated from it
