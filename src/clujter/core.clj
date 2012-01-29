(ns clujter.core
  (:require [clojure.string :as str])
  (:import [java.io BufferedReader FileReader]))

(defn read-data-file
  "Reads a data file, returning an entity count and data points."
  [filename]
  (let [reader (-> filename FileReader. BufferedReader.)
        lines (line-seq reader)
        read-line (fn [line]
                    (map #(Float/parseFloat %)
                         (str/split (str/trim line) #"\s")))
        out (assoc {} :count
                   (Integer/parseInt (first lines)))]
    (assoc out :data
           (map read-line (rest lines)))))

(defn print-results
  "Prints the results of a clustering run."
  [results]
  (doseq [r results]
    (println (str "Cluster with centroid: " (vec (first r))))
    (doseq [p (second r)]
      (println (str (vec p))))
    (println "")))
          
