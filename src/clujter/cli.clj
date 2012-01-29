(ns clujter.cli
  (:use [clujter core kmeans])
  (:gen-class))

(defn -main
  [& args]
  (let [filename (first args)
        dataset (read-data-file filename)
        results (k-cluster (dataset :data) (dataset :count))]
    (println (str "\nResults of running k-means clustering on file: "
                  filename
                  "\n"))
    (print-results results)))


