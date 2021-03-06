(ns clujter.k-means
  (:use [clujter.core]))

(defn nearest-centroid
  "Finds the nearest centroid to the vector."
  [vector centroids]
  (apply min-key
         #(euclidean-distance vector %)
         centroids))

(defn group-with-nearest-centroid
  "Returns a map containing points grouped with their nearest centroid."
  [vectors centroids]
  (group-by #(nearest-centroid % centroids) vectors))

(defn calculate-centroid
  "Calculates the central point of the given points."
  [vectors]
  (let [c (count vectors)]
    (map #(float (/ % c)) (apply map + vectors))))

(defn k-cluster
  "Performs the k-means clustering on the vectors, and returns a map of
  centroids to points."
  [vectors cluster-count]
  (loop [centroids (pick-random vectors cluster-count)]
    (let [clusters (group-with-nearest-centroid vectors centroids)
          new-centroids (map #(calculate-centroid (second %)) clusters)]
      (if (= (set centroids) (set new-centroids))
        clusters
        (recur new-centroids)))))
