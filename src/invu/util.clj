(ns invu.util
    (:require [clojure.set :as set]))

(defn dissoc-in [nested-map nested-keys rm-key]
    (update-in nested-map nested-keys dissoc rm-key))

(defn state-union [map field item]
    (update map field set/union item))

(defn state-disj [map field item]
    (update map field disj item))

(defn rand-range [lower-bound upper-bound]
    "Generates number between lower-bound (inclusive) and upper bound (exclusive)"
    (+ lower-bound (rand-int (- upper-bound lower-bound))))
  
(defn rand-offset [lower-bound offset]
    "Generates number between lower-bound (inclusive) and upwards by offset (exclusive)"
    (rand-range lower-bound (+ lower-bound offset)))