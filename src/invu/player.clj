(ns invu.player
    (:require [invu.util :as util]))

(defrecord Player [id path-travelled will-to-live age])

(defn init-player [name]
    (map->Player {:id name :path-travelled (atom []) :will-to-live (rand-int 10) :age 0}))

(defn decide-jump [player]
    "Given a player on the lower platform decides if she wants to jump ship."
    ; Currently a random decision weighted on will-to-live.
    ; Clojure is dynamically typed so this black magic is legal
    (assert (empty? @(:path-travelled player)))
    (if (< (* (rand-int 2) (:will-to-live player)) 5)
        nil
        (:id player)))

(defn jump [player]
    "A brave player will jump to the next step."
    (swap! (:path-travelled player) conj (rand-int 2)))