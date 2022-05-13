(ns invu.player
    (:require [invu.util :as util]))

(defonce cooperation-thresholds
    {
        0.2 :VLO
        0.4 :LO
        0.6 :MID
        0.8 :HI
        1.0 :VHI
    })

(defonce aggression-thresholds
    {
        0.2 :VLO
        0.4 :LO
        0.6 :MID
        0.8 :HI
        1.0 :VHI
    })

(defonce cooperation-to-min-common-cooperation
    {
        :VLO    0.8
        :LO     0.7
        :MID    0.6
        :HI     0.5
        :VHI    0.4
    })

(defonce aggression-to-min-will-to-live
    {
        :VLO    0.1
        :LO     0.2
        :MID    0.3
        :HI     0.4
        :VHI    0.5
    })

(defprotocol Player
    (will-move [player common-knowledge common-cooperation log?])
    (move [player common-knowledge]))

(defrecord Random [id location will-to-live aggression cooperation decision] Player
    (will-move [player common-knowledge common-cooperation log?]
        (let 
            [fuzzy-cooperation 
                (util/fuzzy-label cooperation-thresholds @(:cooperation player))
             fuzzy-aggression 
                (util/fuzzy-label aggression-thresholds @(:aggression player))
             min-common-cooperation
                (get cooperation-to-min-common-cooperation fuzzy-cooperation)
             min-will-to-live
                (get aggression-to-min-will-to-live fuzzy-aggression)
             cooperation-desire 
                (util/desire min-common-cooperation common-cooperation)
             will-to-live-desire
                (util/desire min-will-to-live @(:will-to-live player))]
            (when log?
                (let [will-jump 
                        (or (contains? common-knowledge (inc @(:location player))) 
                            (and (pos? cooperation-desire) (pos? will-to-live-desire)))]
                    (println
                        (:id player) 
                        @(:cooperation player)
                        @(:aggression player)
                        @(:will-to-live player)
                        fuzzy-cooperation
                        fuzzy-aggression
                        cooperation-desire
                        will-to-live-desire
                        common-cooperation
                        will-jump)))
            (cond
                (contains? common-knowledge (inc @(:location player))) 
                    [1.0 player]
                (and (pos? cooperation-desire) (pos? will-to-live-desire))
                    [(+ cooperation-desire will-to-live-desire) player])))

    (move [player common-knowledge]
        (let [location @(:location player)
          knowledge-available (contains? common-knowledge (inc location))
          next-step (if knowledge-available (get common-knowledge (inc location))
                                            (rand-int 2))]
            (reset! (:decision player) next-step)
            (swap! (:location player) inc))))

;; Common Cooperation [0, 1]
;; Mean average of how many moves have been made so far.
;; ==> moves-made++ if any player jumps in that tick
;; ==> common-cooperation = moves-made / current-tick
;; Changes in common cooperation updates individual agent's cooperation
;; 
;; Cooperation [0, 1]
;; How willing is an agent to follow the social trend? 
;; 
;; Aggression [0, 1]
;; How "selfish" is the agent (primarily focussed on self-preservation)
;; Could this be influenced by death/tick * proportion of unfortuante jumps
;; 
;; Will-to-live [0, 1]
;; How much certainty of death can the agent tolerate?
;; 
;; Min common-cooperation / Min will-to-live to jump
;; C \ A     | VHi (1.0) | Hi (0.8)  | Mid (0.6) |  Lo (0.4) |  VLo (0.2)
;; VHi (1.0) | 0.4 / 0.8 | 0.4 / 0.7 | 0.4 / 0.6 | 0.4 / 0.5 | 0.4 / 0.4
;; Hi  (0.8) | 0.5 / 0.8 | 0.5 / 0.7 | 0.5 / 0.6 | 0.5 / 0.5 | 0.5 / 0.4
;; Mid (0.6) | 0.6 / 0.8 | 0.6 / 0.7 | 0.6 / 0.6 | 0.6 / 0.5 | 0.6 / 0.4
;; Lo  (0.4) | 0.7 / 0.8 | 0.7 / 0.7 | 0.7 / 0.6 | 0.7 / 0.5 | 0.7 / 0.4
;; VLo (0.2) | 0.8 / 0.8 | 0.8 / 0.7 | 0.8 / 0.6 | 0.8 / 0.5 | 0.8 / 0.4

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
