(ns invu.player
    (:require [invu.util :as util]))

(defprotocol Player
    (decide-jump [player common-knowledge timer num-players])
    (jump [player common-knowledge])
    (move [player common-knowledge]))

(defrecord Random [id location will-to-live decision] Player
    (decide-jump [player common-knowledge _ _]
        (if (or (>= (:will-to-live player) 0) (not (empty? common-knowledge)))
            player
            nil))

    (jump [player common-knowledge]
        (if (not (empty? common-knowledge)) 
            (first common-knowledge) 
            (let [choice (rand-int 2)]
                (reset! (:decision player) choice)
                choice)))

    (move [player common-knowledge]
        (let [location @(:location player)
          knowledge-available (contains? common-knowledge (inc location))
          will-jump (or knowledge-available (>= (:will-to-live player) 0))
          next-step (if knowledge-available (get common-knowledge (inc location))
                                            (rand-int 2))]
        ;; (println "PLAYER:" (:id player) "KNOWLEDGE AV:" knowledge-available "WILL JUMP:" will-jump "NEXT STEP:" next-step "WILL TO LIVE:" (:will-to-live player))
            (if will-jump 
                (do
                    (reset! (:decision player) next-step)
                    next-step) 
                nil))))

;; (defn perfect-jump [player _]
;;     "A brave player will jump to the next step."
;;     (reset! (:decision player) 0)
;;     0)

;; (defn perfect-move [player common-knowledge]
;;     "A small step for one player, one giant leap for playerkind.
;;     Players should follow common knowledge if available.
;;     If will-to-live > 5, will move to a random direction.
;;     Returns true if player moved else false."
;;     (let [location @(:location player)
;;           next-step (get common-knowledge (inc location))]
;;         (println "AT LOCATION " location " NEXT STEP IS " next-step)
;;         (reset! (:decision player) next-step)
;;         next-step))
