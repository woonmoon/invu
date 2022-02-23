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

(defn move [player common-knowledge]
    "A small step for one player, one giant leap for playerkind.
    Players should follow common knowledge if available.
    If will-to-live > 5, will move to a random direction.
    Otherwise player will not move."
    (let [path (:path-travelled player)
          player-moves (count @path)]
        (if (< player-moves (count common-knowledge))
            (swap! path conj (nth common-knowledge player-moves))
            (when (< (:will-to-live player) 5) 
                (swap! path conj (rand-int 2))))
    )
)