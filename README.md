clujter
=======

Clujter is a holding project for implementations of machine learning algorithms written in [Clojure](http://clojure.org). All suggestions and criticisms are very welcome.

Dependencies
------------

To build Clujter, you must first have [Leiningen](https://github.com/technomancy/leiningen) installed.

Usage
-----

    lein deps
    lein run resources/kmeans-data-1.txt

Output
------

    Results of running k-means clustering on file: resources/kmeans-data-1.txt

    Cluster with centroid: [8.333333 6.0]
    [8.0 5.0]
    [8.0 6.0]
    [9.0 7.0]

    Cluster with centroid: [22.5 20.0]
    [23.0 25.0]
    [22.0 15.0]

    Cluster with centroid: [2.0 3.25]
    [4.0 6.0]
    [2.0 4.0]
    [1.0 2.0]
    [1.0 1.0]

License
-------

Copyright (C) 2012 Josh Fleming

Distributed under the Eclipse Public License, the same as Clojure.
