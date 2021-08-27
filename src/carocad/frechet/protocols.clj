(ns carocad.frechet.protocols
  "protocols partially required for the frechet distance computations")

(defprotocol Distance
  (distance [object-1 object-2] "computes the distance between two points"))
