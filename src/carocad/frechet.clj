(ns carocad.frechet
  (:require [clojure.core.matrix :as matrix]
            [carocad.frechet.partial :as partial]
            [carocad.frechet.shared :as common]
            [clojure.data :as data]))

;; TODO: replace the generative testing with Clojure's Spec
;; TODO: check viability of replacing core.matrix with a simple vector of vector
;; TODO: check viability of replacing the CA computation with an approach similar to
;;       https://github.com/LightTable/Clojure/pull/94/files))
;; TODO: refactor library into (distance P Q), (coupling P Q) and (shortest-path P Q)
;; TODO: check viability of providing an N to 1 frechet distance by allocating
;;       a single p2p-dist and CA matrix that could be reused for the computation
;; TODO: remove the legacy namespace, there is no need for it anymore
;; TODO: refactor the refine function to be able to interpolate both vectors and
;;       hash-maps (provide a protocol for it ;)
;; TODO: refactor the README accordingly

;; reexposed here for convenience
(def euclidean "euclidean distance for n-dimensional points" matrix/distance)

(defn distance
  "Compute the discrete frechet distance between two curves. P and Q can be
  arbitrary sequential collections.
  dist-fn is a function used to evaluate the distance between any two
  points of P and Q."
  [P Q dist-fn]
  (let [p2p-dist   (common/point-distance P Q dist-fn)
        link       (common/link-matrix p2p-dist)]
        ;coupling   (common/find-sequence (:CA link))]
    (:CA link)))
    ;{:dist (:dist link) :couple coupling}))

(defn- next-row
  [previous-row dist-fn pi Q]
  ;; bootstrap first value due to custom rules
  (let [init-val [(max (get previous-row 0)
                       (dist-fn pi (get Q 0)))]]
    (reduce (fn [current-row [diagonal above qi]]
              (conj current-row (max (min diagonal above (peek current-row))
                                     (dist-fn pi qi))))
            init-val
            (map vector previous-row ;; diagonal
                       (rest previous-row) ;; above
                       (rest Q))))) ;; qi i >= 1

(defn distance2
  [P Q dist-fn]
  (let [init-val (dist-fn (get P 0) (get Q 0))
        init-row (reduce (fn [current-row pi]
                           (conj current-row (max (peek current-row)
                                                  (dist-fn pi (get Q 0)))))
                         [init-val]
                         (rest P))]
    (reduce (fn [rows pi] (conj rows (next-row (peek rows) dist-fn pi Q)))
            ;[(into [] (repeat (dec (count P)) 0))]
            [init-row]
            (rest P))))

(let [P  [[1 2]
          [3 4]
          [5 6]]
      Q  [[1 2]
          [3 4]
          [5 6]]]
  (time (distance P Q euclidean))
  (distance2 P Q euclidean))


(map #(identity %2) (cons nil "hello") (range))

(defn partial-distance
  "Compute the partial frechet distance among P and Q. The partial distance is
  calculated as the frechet distance among R and T, where R and T are the longest
  continuous sub-curves from P and Q that minimize the frechet distance.
  dist-fn is a function used to evaluate the distance between any two
  points of P and Q."
  [P Q dist-fn]
  (let [p2p-dist      (common/point-distance P Q dist-fn)
        [starts ends] (partial/find-boundaries p2p-dist)
        all-bounds    (map #(apply concat %) (partial/cartesian starts ends))
        bounds        (filter partial/valid-bounds? all-bounds)
        frechets      (partial/part-curve-dist p2p-dist bounds)]
    (apply min-key :dist frechets)))
