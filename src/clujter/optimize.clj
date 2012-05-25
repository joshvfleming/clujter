(ns clujter.optimize
  (:use incanter.core))

(defn gradient-descent
  "Performs gradient descent, returning the learned theta and cost history."
  [cost-f X y theta alpha num-iters]
  (loop [theta theta
         i num-iters
         cost-history []]
    (let [[J grad] (cost-f X y theta)
          theta (minus theta (mult alpha grad))
          cost-history (conj cost-history J)]
      (if (= i 1)
        [theta cost-history]
        (recur theta (dec i) cost-history)))))

(defn sigmoid
  "Sigmoid function."
  [x]
  (let [f (fn [t] (/ 1 (+ 1 (Math/exp (- t)))))
        matrix-f (fn [t] (div 1 (plus 1 (exp (mult t -1)))))]
    (cond (matrix? x) (matrix-f x)
          (coll? x) (map f x)
          :else (f x))))

(defn sigmoid-gradient
  "Sigmoid gradient."
  [x]
  (mult (sigmoid x) (minus 1 (sigmoid x))))

(defn nn-cost
  "Simple neural network cost function."
  [X y theta lambda labels]
  true)