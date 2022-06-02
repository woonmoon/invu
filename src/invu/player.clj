(ns invu.player
    (:require 
        [invu.util :as util]
        [invu.logger :as log]))

(defonce fuzzy-thresholds
    {
        0.2 :VLO
        0.4 :LO
        0.6 :MID
        0.8 :HI
        1.0 :VHI
    })

(defonce cooperation->min-common-cooperation
    {
        :VLO    0.8
        :LO     0.7
        :MID    0.6
        :HI     0.5
        :VHI    0.4
    })

(defonce aggression->min-will-to-live
    {
        :VLO    0.1
        :LO     0.2
        :MID    0.3
        :HI     0.4
        :VHI    0.5
    })

(defprotocol Player
    (will-move [player common-cooperation next-step-known? panic?])
    (move [player common-knowledge])
    (update-cooperation [player delta-common-cooperation])
    (update-will-to-live [player delta-chance-of-death])
    (update-aggression [player delta-jump-misfortune])
    (update-player 
        [player delta-chance-of-death delta-jump-misfortune delta-common-cooperation]))

;; Common Cooperation [0, 1]
;; Mean average of how many moves have been made so far.
;; ==> moves-made++ if any player jumps in that tick
;; ==> common-cooperation = moves-made / current-tick
;; There is a flaw with this: consider the case where you are the last surviving agent.
;; Changes in common cooperation updates individual agent's cooperation
;; 
;; Cooperation [0, 1]
;; How willing is an agent to follow the social trend?
;; update-threshold = delta common-cooperation
;; if update-threshold > 0:
;;      cooperation = cooperation + 0.2 * (1 - cooperation)
;; else:
;;      cooperation = cooperation - 0.2 * cooperation
;; 
;; Aggression [0, 1]
;; How "selfish" is the agent (primarily focussed on self-preservation)
;; Could this be influenced by death/tick * proportion of unfortunate jumps
;; update-threshold = delta (death-count / ticks)
;; if update-threshold > 0:
;;      cooperation = cooperation + 0.2 * (1 - cooperation)
;; else:
;;      cooperation = cooperation - 0.2 * cooperation
;; 
;; Will-to-live [0, 1]
;; How much certainty of death can the agent tolerate?
;; update-threshold = delta chance-of-death
;; if update-threshold > 0:
;;      will-to-live = will-to-live + 0.2 * (1 - will-to-live)
;; else:
;;      will-to-live = will-to-live - 0.2 * will-to-live
;; 
;; Min common-cooperation / Min will-to-live to jump
;; C \ A     | VHi (1.0) | Hi (0.8)  | Mid (0.6) |  Lo (0.4) |  VLo (0.2)
;; VHi (1.0) | 0.4 / 0.8 | 0.4 / 0.7 | 0.4 / 0.6 | 0.4 / 0.5 | 0.4 / 0.4
;; Hi  (0.8) | 0.5 / 0.8 | 0.5 / 0.7 | 0.5 / 0.6 | 0.5 / 0.5 | 0.5 / 0.4
;; Mid (0.6) | 0.6 / 0.8 | 0.6 / 0.7 | 0.6 / 0.6 | 0.6 / 0.5 | 0.6 / 0.4
;; Lo  (0.4) | 0.7 / 0.8 | 0.7 / 0.7 | 0.7 / 0.6 | 0.7 / 0.5 | 0.7 / 0.4
;; VLo (0.2) | 0.8 / 0.8 | 0.8 / 0.7 | 0.8 / 0.6 | 0.8 / 0.5 | 0.8 / 0.4
;; 
;; What particular initial configurations of the agent config lead to what outcome of what
;; state.
;; Suppose you could record this data, how is a next population going to react/evolve.
;; Look at yourself and say classifier- what's going to happen to us?
;; Do we go ahead and do this or do we change?
(defrecord Random [id will-to-live cooperation aggression] Player
    (will-move [player common-cooperation next-step-known? panic?]
        (let 
            [fuzzy-cooperation 
                (util/fuzzy-label fuzzy-thresholds (:cooperation player))
             fuzzy-aggression 
                (util/fuzzy-label fuzzy-thresholds (:aggression player))
             min-common-cooperation
                (fuzzy-cooperation cooperation->min-common-cooperation)
             min-will-to-live
                (fuzzy-aggression aggression->min-will-to-live)
             cooperation-desire 
                (util/desire min-common-cooperation common-cooperation)
             will-to-live-desire
                (util/desire min-will-to-live (:will-to-live player))]
            (cond
                next-step-known?
                    [(:id player) 1.0]
                (and (pos? cooperation-desire) (pos? will-to-live-desire))
                    [(:id player) (+ cooperation-desire will-to-live-desire)]
                (and panic? (zero? (rand-int 1)))
                    [(:id player) 0.5])))

    (update-cooperation [player delta-common-cooperation]
        (util/reinforce-value 
            (:cooperation player) 
            delta-common-cooperation 
            0.2 
            0))
    
    (update-will-to-live [player delta-chance-of-death]
        (util/reinforce-value
            (:will-to-live player)
            delta-chance-of-death
            0.2
            0))
            
    (update-aggression [player delta-jump-misfortune]
        (util/reinforce-value
            (:aggression player)
            delta-jump-misfortune
            0.2
            0))

    (update-player 
        [player delta-chance-of-death delta-jump-misfortune delta-common-cooperation]
        (->Random 
            (:id player) 
            (update-will-to-live player delta-chance-of-death)
            (update-aggression player delta-jump-misfortune)
            (update-cooperation player delta-common-cooperation))))