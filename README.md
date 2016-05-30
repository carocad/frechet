# frechet-dist
[![Build Status](https://travis-ci.org/carocad/frechet-dist.svg?branch=master)](https://travis-ci.org/carocad/frechet-dist)
[![License](https://img.shields.io/badge/license-LGPL%20v3-blue.svg)](https://github.com/carocad/frechet-dist/blob/master/LICENSE)

A Clojure library to calculate the discrete [Fréchet distance](https://en.wikipedia.org/wiki/Fr%C3%A9chet_distance) of two polygonal curves as
stated by [Eiter and Mannilla](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.937&rep=rep1&type=pdf).
The curves may be N-dimmensional and need not be the same length.

## Usage
[![Clojars Project](http://clojars.org/frechet-dist/latest-version.svg)](http://clojars.org/frechet-dist)

Simply create matrices with the ordered set of points that describe the curves.
The frechet-dist function returns a hash-map with the discrete frechet distance (:dist) and the coupling sequence (:couple). The Euclidean distance is used as the default metric but you can use an arbirtrary distance function if you pass it as third argument.
```Clojure
(ns example.core
  (:require [frechet-dist.core :as frechet]))

  (frechet/distance [[1 2] [3 4]]
                    [[5 6] [7 8] [9 0]])
;;=> {:dist 7.211102550927978, :couple ([0 0] [1 1] [1 2])}
```
It is also possible to compute the partial discrete frechet distance between two curves. This is calculated as the longest section of *both* curves that minimizes the frechet distance. This is specially useful to extract a common section of overlaping curves. An arbirtrary distance is also optional. Please note that the partial frechet distance is not a metric function and there is no formal definition for it.

```Clojure
(ns example.core
  (:require [frechet-dist.core :as frechet]))

(frechet/partial-distance [[1 2] [3 4]]
                          [[1 2.5] [2.9 4.3] [5 7]])
;;=> {:dist 0.5, :couple ([0 0] [1 1])}
```

Additionally, it is also possible to re-sample the curves in case you might want to improve the precision of the calculation. The *refine* function resides in the *frechet-dist.sampler* namespace.
```Clojure
(refine [[0 3] [0 4] [0 5]] 0.3)
;[[0.0 3.0] [0.0 3.25] [0.0 3.5] [0.0 3.75] [0.0 4.0] [0.0 4.25] [0.0 4.5] [0.0 4.75] [0 5]]
```

## License

Copyright © 2015 Camilo Roca

Distributed under the LGPL v3.
