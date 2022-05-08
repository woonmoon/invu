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

(defn one-and-only [coll] {:pre [(nil? (next coll))]}
    "Returns the one and only item in a list, throws if there is more than one item." 
    (first coll))

(defmacro fuzzy-classify [bounds label x]
    "Returns label if x is within bounds otherwise returns nil"
    `(let [x# ~x
            upper# (first ~bounds)
            lower# (second ~bounds)]
            (when (and (<= upper# x#) (< x# lower#))
                    ~label)))
