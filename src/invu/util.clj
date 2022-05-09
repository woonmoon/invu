(ns invu.util
    (:require [clojure.set :as set]))

(defn other-direction [direction]
    (if (zero? direction) 1 0))

(defn dissoc-in [nested-map nested-keys rm-key]
    (update-in nested-map nested-keys dissoc rm-key))

(defn state-union [map field item]
    (update map field set/union item))

(defn state-disj [map field item]
    (update map field disj item))

(defn state-replace [map field item]
    (update map field (fn [_] item)))
(defn rand-range [lower-bound upper-bound]
    "Generates number between lower-bound (inclusive) and upper bound (exclusive)"
    (+ lower-bound (rand-int (- upper-bound lower-bound))))
  
(defn rand-offset [lower-bound offset]
    "Generates number between lower-bound (inclusive) and upwards by offset (exclusive)"
    (rand-range lower-bound (+ lower-bound offset)))

(defmacro fuzzy-label [thresholds x]
    "Returns correct fuzzy label given thresholds and score x."
    `(let [ x# ~x
            thresholds# (into (sorted-map) ~thresholds)]
                (first 
                    (remove nil? 
                        (map #(when (> (first %) x#) (second %)) thresholds#)))))

(defmacro desire [minimum-threshold x]
    "Returns how much the agent wants to act."
    `(let [x# ~x
            minimum-threshold# ~minimum-threshold]
        (- x# minimum-threshold#)))