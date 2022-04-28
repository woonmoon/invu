(ns invu.core
  (:import (javax.swing JPanel))
  (:require [invu.util :as util]
            [invu.player :as players]
            [invu.domain :as domain]
            [invu.logger :as log])
  (:gen-class))

(defonce tempered-steps (atom {1 0, 2 1, 3 0, 4 1, 5 0, 6 0}))

(defonce new-state
  (atom { :active-players {}
          :dead-players #{}
          :survivors #{}
          :common-knowledge {}
          :tick 0 }))

(defn init-state [state num-steps]
  (let [num-entries (inc num-steps)
        bridge (zipmap 
                  (range num-entries)
                  (take num-entries (repeat #{})))]
    (swap! state assoc :active-players bridge)))

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
          state
        )))))

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
        (when-let [next-step (next-step step bridge)]
          (when-let [player-move (players/move player (:common-knowledge state))]
            (swap! (:location player) inc)
            (swap! bridge update step disj player)
            (swap! bridge update next-step conj player)
            ))))
    ;; (println "BRIDGE: " (into (sorted-map) @bridge))
    (assoc state :active-players (merge {0 platform} (into (sorted-map) @bridge)))))

(defn find-leading-step [active-players]
  ; (println "ACTIVE PLAYERS: " active-players)
  (first 
    (first 
      (filter #(not (empty? (second %))) (reverse active-players)))))

(defn kill [state step player]
  (swap! state #(update-in % [:active-players step] disj player))
  (swap! state #(update-in % [:dead-players] conj player))
)

; Nobody stepped forward
; Somebody stepped forward and died
; Somebody stepped forward and survived
; Somebody made it to the end of the bridge
(defn find-correct-step [state tempered-steps]
  (if (= (:common-knowledge @state) @tempered-steps)
    (let [last-step (first (last @tempered-steps))
          surviving-player (first (get-in @state [:active-players last-step]))]
      (swap! state #(update-in % [:active-players last-step] disj surviving-player))
      (swap! state #(update-in % [:survivors] conj surviving-player))
      nil)
    (let [leading-step (find-leading-step (into (sorted-map) (:active-players @state)))
          leading-player (first (get-in @state [:active-players leading-step]))
          correct-step (get @tempered-steps leading-step)
          common-knowledge (:common-knowledge @state)]
        (if (or (= 0 leading-step) (contains? common-knowledge leading-step))
          nil
          (if (= correct-step @(:decision leading-player))
            [leading-step correct-step]
            (do
              (kill state leading-step leading-player)
              [leading-step (util/other-direction @(:decision leading-player))]))))
  ))

(defn end-of-tick [state tempered-steps]
  (when-let [[step knowledge] (find-correct-step state tempered-steps)]
    (swap! state assoc-in [:common-knowledge step] knowledge)
    (println "COMMON KNOWLEDGE SO FAR " (:common-knowledge @state))
    ; (swap! state #(into (sorted-map) (:common-knowledge %)))
  )
  (swap! state update :tick inc))

;jumped-state and state
(defn tick [state tempered-steps]
  (swap! state maybe-move)
  (swap! state maybe-jump)
  (end-of-tick state tempered-steps))

(defn -main []
  (init-state new-state 6)
  (spawn-players new-state 10)
  (println "********INITIAL STATE********")
  (log/log-state new-state)
  (dotimes [t 20]
    (tick new-state tempered-steps)
    (log/log-state new-state)
  )
  (shutdown-agents)
)
