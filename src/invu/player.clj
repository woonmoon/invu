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
    (update-cooperation [player delta-common-cooperation])
    (update-will-to-live [player delta-chance-of-death])
    (update-aggression [player delta-jump-misfortune]))

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
(defrecord Random [id location will-to-live aggression cooperation] Player
    (will-move [player common-knowledge common-cooperation]
        (let 
            [fuzzy-cooperation 
                (util/fuzzy-label cooperation-thresholds (:cooperation player))
             fuzzy-aggression 
                (util/fuzzy-label aggression-thresholds (:aggression player))
             min-common-cooperation
                (get cooperation-to-min-common-cooperation fuzzy-cooperation)
             min-will-to-live
                (get aggression-to-min-will-to-live fuzzy-aggression)
             cooperation-desire 
                (util/desire min-common-cooperation common-cooperation)
             will-to-live-desire
                (util/desire min-will-to-live (:will-to-live player))
             will-jump
                (or (contains? common-knowledge (inc (:location player))) 
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
                (contains? common-knowledge (inc (:location player))) 
                    [(:id player) 1.0]
                (and (pos? cooperation-desire) (pos? will-to-live-desire))
                    [(:id player) (+ cooperation-desire will-to-live-desire)])))

    ;; (update-cooperation [player delta-common-cooperation]
    ;;     (let [new-cooperation 
    ;;             (util/reinforce-value 
    ;;                 (:cooperation player) 
    ;;                 delta-common-cooperation 
    ;;                 0.2 
    ;;                 0)]
    ;;         (reset! (:cooperation player) new-cooperation)))
    
    ;; (update-will-to-live [player delta-chance-of-death]
    ;;     (let [new-will-to-live
    ;;             (util/reinforce-value
    ;;                 @(:will-to-live player)
    ;;                 delta-chance-of-death
    ;;                 0.2
    ;;                 0)]
    ;;         (reset! (:will-to-live player) new-will-to-live)))
            
    ;; (update-aggression [player delta-jump-misfortune]
    ;;     (let [new-aggression
    ;;             (util/reinforce-value
    ;;                 @(:aggression player)
    ;;                 delta-jump-misfortune
    ;;                 0.2
    ;;                 0)]
    ;;         (reset! (:aggression player) new-aggression)))
            )