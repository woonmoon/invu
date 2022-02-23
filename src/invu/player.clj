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

; TODO: merge functions jump and move, they're the same thing.
(defn jump [player]
    "A brave player will jump to the next step."
    (let [decision (rand-int 2)]
        (swap! (:path-travelled player) conj decision)
        decision))

(defn move [player common-knowledge]
    "A small step for one player, one giant leap for playerkind.
    Players should follow common knowledge if available.
    If will-to-live > 5, will move to a random direction.
    Returns true if player moved else false."
    (let [path (:path-travelled player)
          player-moves (count @path)
          knowledge-available (< player-moves (count common-knowledge))
          next-step (if knowledge-available (nth common-knowledge player-moves)
                                            (rand-int 2))
          will-jump (or knowledge-available (> (:will-to-live player) 2))]
        (when will-jump
            (swap! path conj next-step))            
        ; TODO: There has got to be a better way of doing this. Consider "do"
        (if will-jump next-step nil)))