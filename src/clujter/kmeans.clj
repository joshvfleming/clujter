(ns clujter.kmeans
  (:use [clujter.core]))

(defn pick-random
  "Chooses a number of random vectors from the collection."
  [vectors c]
  (take c (shuffle vectors)))

(defn get-distance
  "Calculates Euclidean distance between two points."
  [a b]
  (let [x-diff (- (nth a 0) (nth b 0))
        y-diff (- (nth a 1) (nth b 1))]
    (Math/sqrt (+ (Math/pow x-diff 2) (Math/pow y-diff 2)))))

(defn nearest-centroid
  "Finds the nearest centroid to the vector."
  [vector centroids]
  (loop [remaining centroids
         nearest nil
         nearest-dist 0]
      (if (empty? remaining)
        (vec nearest)
        (let [centroid (first remaining)
              dist (get-distance vector centroid)]
          (if (or (nil? nearest) (< dist nearest-dist))
            (recur (rest remaining) centroid dist)
            (recur (rest remaining) nearest nearest-dist))))))

(defn group-with-nearest-centroid
  "Returns a map containing points grouped with their nearest centroid."
  [vectors centroids]
  (loop [remaining vectors
         clusters {}]
    (if (empty? remaining)
      clusters
      (let [vector (first remaining)
            nearest (nearest-centroid vector centroids)
            new-clusters (assoc clusters nearest
                                (vec (conj (clusters nearest) vector)))]
        (recur (rest remaining) new-clusters)))))

(defn calculate-centroid
  "Calculates the central point of the given points."
  [nodes]
  (let [c (count nodes)]
    (map #(float (/ % c)) (apply map + nodes))))

(defn k-cluster
  "Performs the k-means clustering on the vectors, and returns a map of
  centroids to points."
  [vectors cluster-count]
  (loop [centroids (pick-random vectors cluster-count)]
    (let [clusters (group-with-nearest-centroid vectors centroids)
          new-centroids (map #(calculate-centroid (last %)) clusters)]
      (if (= (set centroids) (set new-centroids))
        clusters
        (recur new-centroids)))))
