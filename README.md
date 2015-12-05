# frechet-dist

A Clojure library to calculate the discrete [Fréchet distance](https://en.wikipedia.org/wiki/Fr%C3%A9chet_distance) of two polygonal curves as
stated by [Eiter and Mannilla](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.90.937&rep=rep1&type=pdf).
The curves may be N-dimmensional and need not be the same length.

Currently the point-to-point distance is calculated using the Euclidean distance.

## Usage
[![Clojars Project](http://clojars.org/frechet-dist/latest-version.svg)](http://clojars.org/frechet-dist)

A single function is provided as API. This example uses the core.matrix API
```Clojure
(frechet-dist (array [[1 2] [3 4]])
              (array [[5 6] [7 8]]))
```

## License

Copyright © 2015 Camilo Roca

Distributed under the LGPL v3.
