# frechet-dist

A Clojure library to calculate the discrete [Fréchet distance](https://en.wikipedia.org/wiki/Fr%C3%A9chet_distance) of two polygonal curves as
stated by [Eiter and Mannilla](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.937&rep=rep1&type=pdf).
The curves may be N-dimmensional and need not be the same length.

## Usage
[![Clojars Project](http://clojars.org/frechet-dist/latest-version.svg)](http://clojars.org/frechet-dist)

Simply create matrices with the ordered set of points that describe the curves.
The frechet-dist function returns a vector with the discrete frechet distance as the first element and the coupling sequence as the second element. The Euclidean distance is used as the default metric but you can use an arbirtrary distance function if you pass it as third argument.
```Clojure
(frechet-dist [[1 2] [3 4]]
              [[5 6] [7 8] [9 0]])
;[7.211102550927978 ([0 0] [1 0] [1 1] [1 2])]
```
It is also possible to compute the partial discrete frechet distance between two curves. This is calculated as the minimum section of *both* curves that minimizes the frechet distance. An arbirtrary distance is also optional.

```Clojure
(partial-frechet-dist [[1 2.0] [3.0 4.0]]
                      [[1 2.5] [2.9 4.3] [5 7]])
; [0.5 ([0 0] [1 1])]
```

Additionally, it is also possible to re-sample the curves in case you might want to improve the precision of the calculation. The *refine* function resides in the *frechet-dist.sampler* namespace. Currently it is necessary to use the core.matrix vectorz implementation to use this function.

```Clojure
(refine (matrix :vectorz [[0 3] [0 4] [0 5]]) 0.5)
;[[0.0 3.0] [0.0 3.25] [0.0 3.5] [0.0 3.75] [0.0 4.0] [0.0 4.25] [0.0 4.5] [0.0 4.75] [0.0 5.0]]
```

## License

Copyright © 2015 Camilo Roca

Distributed under the LGPL v3.
