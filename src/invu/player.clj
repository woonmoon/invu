(ns invu.player
    (:require [invu.util :as util]))

(defrecord Player [id location will-to-live decision])

(defn init-player [name]
    (map->Player {:id name :location (atom 0) :will-to-live (rand-int 10) :decision (atom nil)}))

(defn decide-jump [player common-knowledge]
    "Given a player on the lower platform decides if she wants to jump ship."
    ; Currently a random decision weighted on will-to-live.
    ; Clojure is dynamically typed so this black magic is legal
    (if (or (> (:will-to-live player) 0) (not (empty? common-knowledge)))
        (do
            (println "Player " (:id player) " IS WILLING TO LEAP")
            player)
        nil))

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
          knowledge-available (contains? common-knowledge (inc location))
          will-jump (or knowledge-available (> (:will-to-live player) 0))
          next-step (if knowledge-available (get common-knowledge (inc location))
                                            (rand-int 2))]
        (if will-jump 
            (do
                (println "WILLING TO JUMP")
                (reset! (:decision player) next-step)
                next-step) 
            nil)))

    ;; TODO WRAP BRIDGE IN ATOM