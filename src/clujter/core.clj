(ns clujter.core
  (:require [clojure.string :as str])
  (:import [java.io BufferedReader FileReader]))

(defn pick-random
  "Chooses a number of random vectors from the collection."
  [vectors c]
  (take c (shuffle vectors)))

(defn euclidean-distance
  "Calculates Euclidean distance between vectors."
  [& vectors]
  (let [diffs (apply map - vectors)]
    (Math/sqrt (reduce + (map #(Math/pow % 2) diffs)))))

(defn dot-product
  "Finds the dot product of the vectors."
  [& vectors]
  (reduce + (apply map * vectors)))

(defn pearson-coefficient
  "Calculates the Pearson similarity between two points."
  [a b]
  (let [n (count a)]
    (/ (- (dot-product a b) (/ (* (reduce + a) (reduce + b)) n))
       (Math/sqrt (* (- (dot-product a a) (/ (Math/pow (reduce + a) 2) n))
                     (- (dot-product b b) (/ (Math/pow (reduce + b) 2) n)))))))

(defn read-data-file
  "Reads a data file, returning an entity count and data points."
  [filename]
  (with-open [reader (-> filename FileReader. BufferedReader.)]
    (let [lines (doall (line-seq reader))
          read-line (fn [line]
                      (map #(Float/parseFloat %)
                           (str/split (str/trim line) #"\s")))
          out (assoc {} :count
                     (Integer/parseInt (first lines)))]
      (assoc out :data
             (map read-line (rest lines))))))

(defn print-results
  "Prints the results of a clustering run."
  [results]
  (doseq [r results]
    (println (str "Cluster with centroid: " (vec (first r))))
    (doseq [p (second r)]
      (println (str (vec p))))
    (println "")))
          
