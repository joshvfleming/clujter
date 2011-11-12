(ns clujter.core
  (:use [incanter core stats charts latex datasets])
  (:require [clojure.contrib.math :as math]))

(view (get-dataset :iris))

(def iris (to-matrix (get-dataset :iris)))

(def sepal-lengths (sel iris :cols 0))
(def pedal-lengths (sel iris :cols 2))

(view (scatter-plot sepal-lengths pedal-lengths))

(defn get-vectors
  [col1 col2]
  (vec
   (map #(vec %)
        (partition 2 (interleave col1 col2)))))

(defn pick-random
  [vectors c]
  (take c (shuffle vectors)))

(defn get-distance
  [a b]
  (let [x-diff (- (nth a 0) (nth b 0))
        y-diff (- (nth a 1) (nth b 1))]
    (math/sqrt (+ (Math/pow x-diff 2) (Math/pow y-diff 2)))))

(defn nearest-cluster
  [vector clusters]
  (loop [remaining clusters
         nearest nil
         nearest-dist 0]
      (if (empty? remaining)
        nearest
        (let [cluster (first remaining)
              dist (get-distance vector (cluster :center))]
          (if (or (nil? nearest) (< dist nearest-dist))
            (recur (rest remaining) cluster dist)
            (recur (rest remaining) nearest nearest-dist))))))

(defn calculate-centroid
  [nodes]
  (let [c (count nodes)]
    (map #(/ % c) (eval `(map + ~@nodes)))))

(defn k-cluster
  [vectors cluster-count]
  (loop [centroids (pick-random vectors cluster-count)
         clusters ()]
    (for [v vectors]
      (let [
      println "test")))
