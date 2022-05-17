(ns invu.player
    (:require 
        [invu.util :as util]
        [invu.logger :as log]))

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
    (will-move [player common-knowledge common-cooperation])
    (move [player common-knowledge])
    (update-cooperation [player delta-common-cooperation]))

;; Common Cooperation [0, 1]
;; Mean average of how many moves have been made so far.
;; ==> moves-made++ if any player jumps in that tick
;; ==> common-cooperation = moves-made / current-tick
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

(defrecord Random [id location will-to-live aggression cooperation decision] Player
    (will-move [player common-knowledge common-cooperation]
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
                (util/desire min-will-to-live @(:will-to-live player))
             will-jump
                (or (contains? common-knowledge (inc @(:location player))) 
                    (and (pos? cooperation-desire) (pos? will-to-live-desire)))]
            ;; Tragic, but not the top of my problems.
            (log/log :test-jump
                player
                fuzzy-cooperation
                fuzzy-aggression
                cooperation-desire
                will-to-live-desire
                common-cooperation
                will-jump)
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
            (swap! (:location player) inc)))
    
    (update-cooperation [player delta-common-cooperation]
        (let [new-cooperation 
                (util/reinforce-value 
                    @(:cooperation player) 
                    delta-common-cooperation 
                    0.2 
                    0)]
            (reset! (:cooperation player) new-cooperation)))
    
    ;; (update-aggression [player]
    ;;     )
    )