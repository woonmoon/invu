(ns invu.util
    (:require [clojure.set :as set]))

(defn get-active-players [state]
    (concat (keep identity (vals (:bridge state))) (:platform state)))

(defn rand-range [lower-bound upper-bound]
    (+ lower-bound (rand (- upper-bound lower-bound))))

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

(defmacro one-div [x y]
    "Returns x/y if the divisor is 0 returns 1 (used for updating state thresholds)"
    `(let [x# ~x
            y# ~y]
            (if (zero? y#)
                1
                (/ x# y#))))


(defmacro zero-div [x y]
    "Returns x/y if the divisor is 1 returns 0 (used for updating state thresholds)"
    `(let [x# ~x
            y# ~y]
            (if (zero? y#)
                0
                (/ x# y#))))

(defmacro reinforce-value [value indicator learning-rate threshold]
    `(let [value# ~value
            indicator# ~indicator
            rate# ~learning-rate
            threshold# ~threshold]
        (if (< threshold# indicator#)
            (+ value# (* rate# (- 1 value#)))
            (- value# (* rate# value#)))))