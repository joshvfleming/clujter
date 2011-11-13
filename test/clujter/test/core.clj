(ns clujter.test.core
  (:use [clujter.core])
  (:use [clojure.test]))

(def ^:dynamic *test-data-1* [[1 1] [22 15] [1 2] [23 25] [2 4]])
(def ^:dynamic *test-data-1-clustered*
     {[22.5 20.0] [[22 15] [23 25]],
      [1.3333334 2.3333333] [[1 1] [1 2] [2 4]]})

(defn point-set
  [clusters]
  (set (map #(set (last %)) clusters)))

(defn clusters-equal
  [c1 c2]
  (= (point-set c1) (point-set c2)))

(deftest k-cluster-test
  (let [clusters (k-cluster *test-data-1* 2)]
    (is (= (count clusters) 2))
    (is (clusters-equal *test-data-1-clustered* clusters))))