(ns clujter.test.core
  (:use [clujter core k-means])
  (:use [midje.sweet]))

(def ^:dynamic *test-data-1* [[1 1] [22 15] [1 2] [23 25] [2 4]])
(def ^:dynamic *test-data-1-clustered*
     {[22.5 20.0] [[22 15] [23 25]],
      [1.3333334 2.3333333] [[1 1] [1 2] [2 4]]})

(def ^:dynamic *ratings*
  {"Lisa Rose" {"Lady in the Water" 2.5
                "Snakes on a Plane" 3.5
                "Just My Luck" 3.0
                "Superman Returns" 3.5
                "You, Me and Dupree" 2.5
                "The Night Listener" 3.0}
   "Gene Seymour" {"Lady in the Water" 3.0
                   "Snakes on a Plane" 3.5
                   "Just My Luck" 1.5
                   "Superman Returns" 5.0
                   "The Night Listener" 3.0
                   "You, Me and Dupree" 3.5}})

(defn point-set
  [clusters]
  (set (map #(set (last %)) clusters)))

(defn clusters-equal?
  [c1 c2]
  (= (point-set c1) (point-set c2)))

(facts "k-cluster-test"
  (let [clusters (k-cluster *test-data-1* 2)]
    (count clusters) => 2
    clusters => #(clusters-equal? % *test-data-1-clustered*)))

(facts "pearson-coefficeint-test"
  (let [dataset-1 [2.5 3.5 3.0 3.5 2.5 3.0]
        dataset-2 [3.0 3.5 1.5 5.0 3.5 3.0]]
    (pearson-coefficient dataset-1 dataset-2)
    => (roughly 0.396059017191 1/100000)))