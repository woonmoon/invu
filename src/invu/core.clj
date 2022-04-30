(ns invu.core
  (:import (javax.swing JPanel))
  (:require [clojure.set :as set]
            [invu.util :as util]
            [invu.player :as players]
            [invu.domain :as domain]
            [invu.logger :as log])
  (:gen-class))

(defonce new-state
  (atom { :active-players {}
          :dead-players #{}
          :survivors #{}
          :common-knowledge {}
          :tick 0 
          :tempered-steps {} }))

(defn init-state [state num-steps]
  (let [num-entries (inc num-steps)
        bridge (zipmap 
                  (range num-entries)
                  (take num-entries (repeat #{})))
        tempered-steps (zipmap
                          (range 1 num-entries)
                          (take num-steps (repeatedly #(rand-int 2))))]
    (swap! state assoc :active-players bridge)
    (swap! state assoc :tempered-steps tempered-steps)))

(defn spawn-players [state num-players]
  (let [ids (take num-players (repeatedly #(gensym "Player")))
        players (into #{} (map (partial players/init-player) ids))]
    (swap! state assoc-in [:active-players 0] players)))

(defn maybe-jump [state]
  "I volunteer as tribute!"
  (let [candidates (get-in state [:active-players 0])
        tributes (remove nil? 
                    (map #(players/decide-jump % (:common-knowledge state)) candidates))]
    (if (empty? tributes)
      state
      (let [chosen-one (rand-nth tributes)
            player-move (players/jump chosen-one (:common-knowledge state))
            disjoint-state (update-in state [:active-players 0] disj chosen-one)]
        (if (empty? (get-in state [:active-players 1]))
          (do
            (swap! (:location chosen-one) inc)
            (assoc-in disjoint-state [:active-players 1] #{chosen-one}))
          state)))))

(defn next-step [location bridge]
  "Check if the next step of the bridge is occupied or not."
  (let [next-step (inc location)]
    (if (and (empty? (get @bridge next-step)) (<= next-step (count @bridge)))
      next-step
      nil)))

(defn maybe-move [state]
  (let [platform (get-in state [:active-players 0])
        bridge (atom (into (sorted-map-by >) (dissoc (:active-players state) 0)))
        common-knowledge (:common-knowledge state)]
    (doseq [[step players] @bridge]
      (when-let [player (first players)]
        (println "PLAYER MOVING:" (:id player))
        (when-let [next-step (next-step step bridge)]
          (println "NEXT STEP:" next-step)
          (when-let [player-move (players/move player (:common-knowledge state))]
            (println "PLAYER MOVE:" player-move)
            (swap! (:location player) inc)
            (swap! bridge update step disj player)
            (swap! bridge update next-step conj player)))))
    ;; (println "BRIDGE: " (into (sorted-map) @bridge))
    (assoc state :active-players (merge {0 platform} (into (sorted-map) @bridge)))))

(defn find-leading-step [active-players]
  ; (println "ACTIVE PLAYERS: " active-players)
  (first 
    (first 
      (filter #(not (empty? (second %))) (reverse active-players)))))

(defn kill [state step player]
  (swap! state #(update-in % [:active-players step] disj player))
  (swap! state #(update-in % [:dead-players] conj player)))

(defn survive [state player]
  (swap! state #(update-in % [:active-players (first (last (:tempered-steps @state)))] disj player))
  (swap! state #(update-in % [:survivors] conj player)))

; Nobody stepped forward
; Somebody stepped forward and died
; Somebody stepped forward and survived
; Somebody made it to the end of the bridge

; 1. Check first if 
; 2. 
(defn find-correct-step [state]
  (let [last-step (first (last (:tempered-steps @state)))
        leading-step (find-leading-step (into (sorted-map) (:active-players @state)))
        leading-player (first (get-in @state [:active-players leading-step]))
        correct-step (get-in @state [:tempered-steps leading-step])
        common-knowledge (:common-knowledge @state)]
      (println "TEMPERED STEPS:" (get @state :tempered-steps))
      (println "LEADING STEP:" leading-step "LAST STEP:" last-step "LEADING PLAYER:" (:id leading-player) "CORRECT STEP:" correct-step "PLAYER CHOICE:" @(:decision leading-player))
      (if (or (= 0 leading-step) (and (contains? common-knowledge leading-step) (not= leading-step last-step)))
        nil
        (if (= correct-step @(:decision leading-player))
          (do
            (println "CORRECT STEP IS LEAD DECISION LEADING-STEP:" leading-step "LAST STEP:" last-step)
            (if (= leading-step last-step)
              (do
                (println "CELINE DION")
                (survive state leading-player)
                nil)
              [leading-step correct-step]))
          (do
            (kill state leading-step leading-player)
            [leading-step (util/other-direction @(:decision leading-player))]))))
)

(defn end-of-tick [state]
  (when-let [[step knowledge] (find-correct-step state)]
    (swap! state assoc-in [:common-knowledge step] knowledge)))

;jumped-state and state
(defn tick [state]
  ; (println "BEFORE MAYBE MOVE")
  (swap! state maybe-move)
  ; (println "BEFORE MAYBE JUMP")
  (swap! state maybe-jump)
  ; (println "BEFORE EOT")
  (end-of-tick state)
  (swap! state update :tick inc))

(defn start-simulation [state num-ticks]
  (while (and (< (:tick @state) num-ticks)
              (not (empty? (apply set/union (vals (:active-players @state))))))
    (tick new-state)
    (log/log-state new-state)))

(defn -main []
  (init-state new-state 6)
  (spawn-players new-state 6)
  (log/log-state new-state)
  (start-simulation new-state 50)
  (shutdown-agents)
)
