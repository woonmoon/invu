(ns invu.util
    (:require [clojure.set :as set]))

(defn other-direction [direction]
    (if (zero? direction) 1 0))

(defn dissoc-in [nested-map nested-keys rm-key]
    (update-in nested-map nested-keys dissoc rm-key))

(defn rand-range [lower-bound upper-bound]
    "Generates number between lower-bound (inclusive) and upper bound (exclusive)"
    (+ lower-bound (rand-int (- upper-bound lower-bound))))
  
(defn rand-offset [lower-bound offset]
    "Generates number between lower-bound (inclusive) and upwards by offset (exclusive)"
    (rand-range lower-bound (+ lower-bound offset)))

(defn count-active-players [active-players]
    "Returns number of active players when given a mapping of step to player hashmap"
    (reduce + (map count (vals active-players))))

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

(defmacro safe-div [x y]
    "Returns x/y if the divisor is 0 returns 1 (used for updating state thresholds)"
    `(let [x# ~x
            y# ~y]
            (if (zero? y#)
                1
                (/ x# y#))))

(defmacro reinforce-value [value indicator learning-rate threshold]
    `(let [value# ~value
            indicator# ~indicator
            rate# ~learning-rate
            threshold# ~threshold]
        (if (< threshold# indicator#)
            (+ value# (* rate# (- 1 value#)))
            (- value# (* rate# value#)))))