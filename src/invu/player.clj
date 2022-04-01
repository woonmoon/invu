(ns invu.player
    (:require [invu.util :as util]))

(defrecord Player [id location will-to-live decision])

(defn init-player [name]
    (map->Player {:id name :location (atom 0) :will-to-live (rand-int 10) :decision (atom nil)}))

(defn decide-jump [player common-knowledge]
    "Given a player on the lower platform decides if she wants to jump ship."
    ; Currently a random decision weighted on will-to-live.
    ; Clojure is dynamically typed so this black magic is legal
    (if (or (< (* (rand-int 2) (:will-to-live player)) 5) 
            (not (empty? common-knowledge)))
        nil
        player))

; TODO: merge functions jump and move, they're the same thing.
(defn jump [player common-knowledge]
    "A brave player will jump to the next step."
    (if (not (empty? common-knowledge)) 
        (first common-knowledge) 
        (let [choice (rand-int 2)]
            (reset! (:decision player) choice)
            choice)))

(defn move [player common-knowledge]
    "A small step for one player, one giant leap for playerkind.
    Players should follow common knowledge if available.
    If will-to-live > 5, will move to a random direction.
    Returns true if player moved else false."
    (let [location @(:location player)
          knowledge-available (< location (count common-knowledge))
          will-jump (or knowledge-available (> (:will-to-live player) 2))
          next-step (if knowledge-available (nth common-knowledge location)
                                            (rand-int 2))]
        (println player)
        (when will-jump
            (reset! (:decision player) next-step))
        (println player)            
        (if will-jump next-step nil)))

    ;; TODO WRAP BRIDGE IN ATOM