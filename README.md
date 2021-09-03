# frechet
[![Build Status](https://travis-ci.org/carocad/frechet-dist.svg?branch=master)](https://travis-ci.org/carocad/frechet)
[![Clojars Project](https://img.shields.io/clojars/v/net.clojars.carocad/frechet.svg)](https://clojars.org/net.clojars.carocad/frechet)
[![CljDoc](https://cljdoc.org/badge/net.clojars.carocad/frechet)](https://cljdoc.org/d/net.clojars.carocad/frechet/)

A Clojure(script) library to calculate the discrete [Fréchet distance](https://en.wikipedia.org/wiki/Fr%C3%A9chet_distance)
of two polygonal curves as stated by [Eiter and Mannilla](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.937&rep=rep1&type=pdf).
The curves may be N-dimensional and need not be the same length.

## Usage

Simply create 2D vectors with the points that describe the curves. 
In order to compute the frechet distance you need to provide a metric function
to compute the distance between any point on the curves. The `euclidean` function
is also part of the library for convenience.

```Clojure
(ns example.core
  (:require [carocad.frechet :as frechet]))

(frechet/distance [[1 2] [3 4]]
                  [[5 6] [7 8] [9 0]]
                  frechet/euclidean)
;; #:carocad.frechet{:distance 7.211102550927978, :coupling ([0 0] [1 1] [1 2])}
```

For more information please check the documentation in
[CljDoc](https://cljdoc.org/badge/net.clojars.carocad/frechet).

## License

Copyright © 2015 Camilo Roca

Distributed under the LGPL v3.
