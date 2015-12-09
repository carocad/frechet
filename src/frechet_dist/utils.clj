(ns frechet-dist.utils
  (:require [clojure.core.matrix :refer [mget emax]]
            [frechet-dist.shared :refer [find-sequence]]))

(defn delimiter
  [dir]
  (condp = dir  ;start  end
    :right  (fn [is js ie je] [is       js       ie    (dec je)])
    :left   (fn [is js ie je] [is       (inc js) ie         je])
    :bottom (fn [is js ie je] [is       js       (dec ie)   je])
    :top    (fn [is js ie je] [(inc is) js       ie         je])))

(defn max-leash
  [CA coupling]
  (emax (map #(apply mget CA %) coupling)))

(defn opt-bounds
  [CA bound-to limits]
  (loop [[i-start j-start i-end j-end] limits
         curr-leash                    (max-leash CA (find-sequence CA [i-start j-start i-end j-end]))]
    (let [new-leash                    (max-leash CA (find-sequence CA (bound-to i-start j-start i-end j-end)))]
      ;(println curr-leash new-leash [i-start j-start i-end j-end])
      (if (> curr-leash new-leash)
        (recur (bound-to i-start j-start i-end j-end) new-leash)
        [i-start j-start i-end j-end]))))